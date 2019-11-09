library(tm)
library(ggplot2)
both.urls <- c('http://spo.handball4all.de/Spielbetrieb/?orgGrpID=80','http://spo.handball4all.de/Spielbetrieb/?orgGrpID=79')
for(j in 1:length(both.urls)){
  all.leagues <- readLines(both.urls[j])
  
  sel.male <- unlist(lapply(all.leagues,function(x){grepl("M-",x)}))
  sel.female <- unlist(lapply(all.leagues,function(x){grepl("F-",x)}))
  sel.female.youth <- unlist(lapply(all.leagues,function(x){grepl("wJ",x)}))
  sel.male.youth <- unlist(lapply(all.leagues,function(x){grepl("mJ",x)}))
  
  sel.all <- sel.male | sel.female | sel.female.youth | sel.male.youth
  sel.leagues <- all.leagues[sel.all]
  info.leagues <- strsplit(sel.leagues,'>')
  index.interest <- c(3,4)
  info.leagues <- lapply(info.leagues, function(x)x[index.interest])
  names.leagues <- strsplit(unlist(lapply(info.leagues, function(x)x[2])),"<")
  names.leagues <- unlist(lapply(names.leagues, function(x)x[1]))
  ids.leagues <- strsplit(unlist(lapply(info.leagues, function(x)x[1])),"score=")
  ids.leagues <- unlist(strsplit(unlist(lapply(ids.leagues, function(x)x[2])),'[[:punct:]]'))
  names.leagues <- names.leagues[!is.na(ids.leagues)]
  ids.leagues <- ids.leagues[!is.na(ids.leagues)]
  
  store.folder <- file.path("/home/mscherer/Documents/Handball/hvsaar_stats/stats")
  #store.folder <- file.path("C://Users/Acer/Documents/Handball/stats/test/")
  png.folder <- file.path(store.folder,'pngs')
  pdf.folder <- file.path(store.folder,'pdfs')
  csv.folder <- file.path(store.folder,"csv")
  if(!dir.exists(store.folder)){
    dir.create(store.folder)
    dir.create(png.folder)
    dir.create(pdf.folder)
    dir.create(csv.folder)
  }
  
  for(i in 1:length(names.leagues)){
    league <- names.leagues[i]
    league.id <- ids.leagues[i]
    file <- paste0(both.urls[j],'&score=',league.id,'&all=1')
    file <- readLines(file)
    sel <- unlist(lapply(file,function(x){grepl("sGID",x)}))
    selected <- file[sel]
    splitted <- lapply(selected,function(x){strsplit(x,'href=')})
    splitted <- unlist(lapply(splitted,function(x){x[[1]][3]}))
    splitted <- lapply(splitted,function(x){strsplit(x,"\"")})
    splitted <- unlist(lapply(splitted,function(x){x[[1]][2]}))
    splitted <- splitted[!is.na(splitted)]
    df <- c()
    for(game in splitted){
      doc <- readPDF(control = list(text="-htmlmeta"))(elem=list(uri=game),
                                                       language="en",
                                                       id="id1")
      element <- doc$content[2]
      element <- strsplit(element,'\n')
      element <- element[[1]]
      has.info <- unlist(lapply(element,function(x){grepl(" [[:digit:]] ",x)||grepl("  [[:digit:]] ",x)||grepl("  [[:digit:]][[:digit:]] ",x)||grepl(" [[:digit:]][[:digit:]] ",x)}))
      element <- element[has.info]
      element <- unlist(lapply(element,function(x){substr(x,6,nchar(x))}))
      element <- lapply(element,function(x){strsplit(x," ")})
      names <- unlist(lapply(element,function(x){
         return(paste(x[[1]][1],x[[1]][2]))
      })) 
      goals <- unlist(lapply(element,function(x){
        has.goals <- grepl("[[:digit:]]",x[[1]])&!(grepl("[[:punct:]]",x[[1]]))
        if(any(has.goals)){
          x[[1]][has.goals]
        }else{
          NA
        }
      }))
      df <- rbind(df,cbind(names,goals))
    }
    clean.df <- function(df){
      if(nrow(df)==0){
        return(df)
      }
      invalid.strings <- c("Nr.","Nr. Name","A ","A NA","\n A","B ","B NA","\n B","C ","C NA","\n C","D ","D NA","\n D"," SV")
      is.invalid <- unlist(lapply(as.character(df$names),function(x){any(unlist(lapply(invalid.strings,function(y){y %in% x})))}))
      df <- df[!is.invalid,]
      df
    }
    df <- as.data.frame(df)
    df$goals <- as.numeric(as.character(df$goals))
    df <- clean.df(df)
    if(nrow(df)==0){
      plot <- ggplot()
    }else{
      to.plot <- aggregate(df$goals,by=list(df$names),sum,na.rm=T)
      colnames(to.plot) <- c("Name","Tore")
      to.plot$Name <- factor(to.plot$Name, levels = to.plot$Name[order(to.plot$Tore)])
      file.name <- paste0(league,'.csv')
      write.csv(to.plot,file.path(csv.folder,file.name),row.names = F)
      q <- quantile(to.plot$Tore,.9)
      to.plot <- to.plot[to.plot$Tore>q,]
      plot <- ggplot(to.plot,aes(x=Name,y=Tore))+geom_bar(stat = "identity")+theme(
        axis.text.x = element_text(angle = 90), 
        panel.background = element_rect(fill="white",color="black"),
        panel.grid.major = element_line(color="grey80")
      )+coord_flip()+geom_text(aes(label=Tore),nudge_y = q/10)+ggtitle(paste("Stand:",Sys.Date()))
    }
    file.name <- paste0(league,'.pdf')
    ggsave(file.path(pdf.folder,file.name),plot,device = 'pdf')
    file.name <- paste0(league,'.png')
    ggsave(file.path(png.folder,file.name),plot,device = 'png')
  }
}