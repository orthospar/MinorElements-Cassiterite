setwd("C:/Users/Jason/OneDrive - research.uwa.edu.au/Analysis/EPMA")

require(dplyr)
require(ggplot2)
require(ggtern)

source("coloursafe.R") #load colour scheme
source("C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/EPMA Transects/import.R") #load data

#filter data to only analyses with W above the LOD...
#...order the data so that analyses with higher concentrations of substituting elements plot on top
#(and not buried underneath 'noisy' data)
data <- arrange(filter(data, W.AT.>0), (W.AT.+Fe.AT.+Mn.AT.+Ta.AT.+Nb.AT.)) 

#subset the data into localities (the LOCALITY column)
list2env(split.data.frame(data, data$LOCALITY), envir=.GlobalEnv)

#mean uncertainty on wt% W is +/- 0.007 wt%
#mean detection limit on wt% W is 0.035 wt%
#minimum W measurement is 0.02725 wt%
#maximum W measurement is 1.7745 wt%
#median W measurement is 0.09119 wt%

#alpha scale set to weight points 3x above detection. 

p<-ggtern(data=NULL, aes(x=W.AT., y=(Fe.AT.+Mn.AT.), z=(Ta.AT.+Nb.AT.), alpha=(W.WT.), colour=LOCALITY))+
  scale_colour_manual(values = coloursafe)+
  scale_alpha(limits = c(0.02, 0.105), breaks=c(0.035, 0.07, 0.105), range = c(0, 0.6), guide="none")+
  guides(color = guide_legend(override.aes = list(size=4)))+
  annotate("segment",
           x=c(1/3,1.0,0.5,2/3),xend=c(0.0,0.0,0.0,0.0),
           y=c(2/3,0.0,0.5,1/3),yend=c(0.5,1/3,1/3,1/3),
           z=c(0.0,0.0,0.0,0.0),zend=c(0.5,2/3,2/3,2/3),
           colour="grey50", linetype=1)+
  theme_void()+
  theme_nolabels()+
  theme(panel.background = element_rect(fill = "grey90"),
        legend.key = element_rect(fill = "grey90"),
        plot.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text= element_text(size = 14),
        title = element_text(size = 16),
        legend.justification =c(0,1),
        legend.position = c(0,1))+
  labs(x="", y="", z="", colour=NULL, alpha=NULL)

p1 <- p + geom_point(data=`Saltwater Creek`)+
          geom_point(data=`Blue Tier`)+
          geom_point(data=`Pedra Branca`)+
        labs(title="B): Greisen") 
p2 <- p + geom_point(data=Renison)+ geom_point(data=`Mt Bischoff`)+
        labs(title="D): Skarn")
p3 <- p + geom_point(data=`Sifleetes Reward`)+
        labs(title="A): Pegmatite")
p4 <- p + geom_point(data=Elsmore)+ geom_point(data=Aberfoyle)+
        labs(title="C): Vein-hosted")

w<-6
ggsave("Figures/W1.pdf", p1, width=w, height=w*0.9)
ggsave("Figures/W2.pdf", p2, width=w, height=w*0.9)
ggsave("Figures/W3.pdf", p3, width=w, height=w*0.9)
ggsave("Figures/W4.pdf", p4, width=w, height=w*0.9)