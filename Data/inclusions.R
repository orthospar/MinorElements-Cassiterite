setwd('C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/EPMA Transects')
library(ggplot2)
library(plotly)
bdl.na <- function(EPMA_data){
  EPMA_data <- within(EPMA_data, {
    Si.WT.[Si.WT. < Si.CDL99] <- 0
    Al.WT.[Al.WT. < Al.CDL99] <- 0
    Na.WT.[Na.WT. < Na.CDL99] <- 0
  })
}

#data<-read.csv("170609/170609.csv")
#filter_lines <- c(153,164,176,265,332:335,405,513,605,735,780,797,841,843:844,997:998,1140,1189:1190,1215,1246)

#data<- read.csv("170811/170811.csv")
#filter_lines <-c(59, 72, 99, 237)

#data <- read.csv("170811/170814cont.csv")
#filter_lines <-c(295, 321, 386)

#data <- read.csv("171207/171207.csv")
#filter_lines <- c(142, 152, 163, 205, 227, 248, 253, 270, 276, 278:280, 285, 310, 388, 423, 425, 553:582, 668:697)
#filter_lines <- c(filter_lines, 128, 190, 260, 287, 315, 389)

#data <- read.csv("050831/Lilly_2.csv")
#filter_lines <- c(88, 108)

data <- read.csv("180209/180209.csv")
filter_lines <- c(477:479,831,876,1507:1508,1672,1747:1809,1920)

data <- data[!(data$LINE %in% filter_lines),]
data <- bdl.na(data)

plot<-ggplot(data, aes(x=LINE, colour=factor(SAMPLE)))+
  #geom_line(aes(y=Sn.WT.))+
  #geom_line(aes(y=TOTAL))+
  #geom_line(aes(y=Si.WT.), colour="red")+
  #geom_line(aes(y=Na.WT.), colour="green")+
  #geom_line(aes(y=Al.WT.))+
  #coord_cartesian(ylim=c(0,0.1))+
  #geom_line(aes(y=Nb.WT.))+
  #geom_line(aes(y=Ta.WT.))+
  #geom_line(aes(y=Fe.WT.))+
  #geom_line(aes(y=Mn.WT.))+
  #geom_line(aes(y=Ti.WT.))+
  #geom_line(aes(y=W.WT.))+
  #geom_line(aes(y=Zr.WT.))+
  #geom_line(aes(y=Sn.WT.+Ti.WT.+Fe.WT.+Mn.WT.+Ta.WT.+Nb.WT.))+
  #geom_line(aes(y=TOTAL-O.WT.))+
  geom_point(aes(x=O.WT.+Sn.WT.+Ti.WT.+Fe.WT.+Mn.WT.+Ta.WT.+Nb.WT.+Zr.WT.+W.WT., y=TOTAL, colour=factor(SAMPLE)))
  theme_bw()
plot<- ggplotly(plot)
print(plot)
