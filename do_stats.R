library(tm)
library(ggplot2)
library(gridExtra)
library(grid)
#' M�nner I
#' file <- readLines('http://spo.handball4all.de/Spielbetrieb/?orgGrpID=80&score=27709&teamID=384885')
#' M�nner II
#' file <- readLines('http://spo.handball4all.de/Spielbetrieb/?orgGrpID=80&score=27865&teamID=386501')
#' Verbandsliga komplett
#' file <- readLines('http://spo.handball4all.de/Spielbetrieb/?orgGrpID=80&score=27709&all=1')
#' file <- readLines('http://spo.handball4all.de/Spielbetrieb/?orgGrpID=80&score=27709&all=1')
file <- readLines('http://spo.handball4all.de/Spielbetrieb/?orgGrpID=80&score=27713&all=1')

sel <- unlist(lapply(file,function(x){grepl("sGID",x)}))
selected <- file[sel]
splitted <- lapply(selected,function(x){strsplit(x,'href=')})
splitted <- unlist(lapply(splitted,function(x){x[[1]][3]}))
splitted <- lapply(splitted,function(x){strsplit(x,"\"")})
splitted <- unlist(lapply(splitted,function(x){x[[1]][2]}))
df <- c()
for(game in splitted){
  doc <- readPDF(control = list(text="-htmlmeta"))(elem=list(uri=game),
                                               language="en",
                                               id="id1")
  element <- doc$content[2]
  element <- strsplit(element,'\r')
  element <- element[[1]]
  has.info <- unlist(lapply(element,function(x){grepl("\n [[:alnum:]]",x)||grepl("\n  [[:alnum:]]",x)}))
  element <- element[has.info]
  element <- unlist(lapply(element,function(x){gsub("\n..[[:alnum:]] ","",x)}))
  element <- lapply(element,function(x){strsplit(x," ")})
  names <- unlist(lapply(element,function(x){
    if(x[[1]][1]==""){
      return(paste(x[[1]][2],x[[1]][3]))
    }else{
      return(paste(x[[1]][1],x[[1]][2]))
    }
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
  invalid.strings <- c("Nr.","Nr. Name","A ","A NA","\n A","B ","B NA","\n B","C ","C NA","\n C","D ","D NA","\n D")
  is.invalid <- unlist(lapply(as.character(df$names),function(x){any(unlist(lapply(invalid.strings,function(y){y %in% x})))}))
  df <- df[!is.invalid,]
  df
}
df <- as.data.frame(df)
df$goals <- as.numeric(as.character(df$goals))
df <- clean.df(df)
to.plot <- aggregate(df$goals,by=list(df$names),sum,na.rm=T)
colnames(to.plot) <- c("Name","Tore")
to.plot$Name <- factor(to.plot$Name, levels = to.plot$Name[order(to.plot$Tore)])
sel <- quantile(to.plot$Tore,.9)
to.plot <- to.plot[to.plot$Tore>sel,]
plot <- ggplot(to.plot,aes(x=Name,y=Tore))+geom_bar(stat = "identity")+theme(
  axis.text.x = element_text(angle = 90), 
  panel.background = element_rect(fill="white",color="black"),
  panel.grid.major = element_line(color="grey80")
  #text=element_text(size=5)
)+coord_flip()+geom_text(aes(label=Tore),nudge_y = 1)+
  ggtitle(paste("Stand:",Sys.Date()))
ggsave("C:/Users/Acer/Desktop/VL_stats_18_03.pdf",plot)
