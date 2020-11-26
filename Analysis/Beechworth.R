setwd("C:/Users/Jason/OneDrive - research.uwa.edu.au/Analysis/EPMA")
require(dplyr)
require(ggplot2)
require(ggtern)
require(RColorBrewer)
source("coloursafe.R")

source("C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/EPMA Transects/import.R")
data <- arrange(data, desc(Sn.OXMOL.))
list2env(split.data.frame(data, data$STYLE), envir=.GlobalEnv)

p<-ggtern(data=NULL, aes(x=Ti.AT., y=(Fe.AT.+Mn.AT.), z=(Ta.AT.+Nb.AT.)))+
  scale_colour_manual(values = coloursafe)+
  annotate("segment", x=c(1,1), xend=c(0.0,0.00),
           y=c(0,0), yend=c(0.5,0.33),
           z=c(0,0), zend=c(0.5,0.66),
           colour="grey20", linetype=1)+
  theme_void()+
  theme_nolabels()+
  theme(panel.background = element_rect(fill = "grey90"),
        plot.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        title = element_text(size=16),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10, angle=0),
        legend.justification =c(0,1),
        legend.position = c(0,0.9)
  )+
  labs(x="", y="", z="", colour=expression("SnO"[2]~"(mol%)"))

p<- p+stat_density_tern(geom='polygon', data=Vein, aes(alpha=..level.., fill=factor(STYLE)), bins=7)+
      stat_density_tern(geom='polygon', data=Greisen, aes(alpha=..level.., fill=factor(STYLE)), bins=7)+
      stat_density_tern(geom='polygon', data=Pegmatite, aes(alpha=..level.., fill=factor(STYLE)), bins=7)+
      stat_density_tern(geom='polygon', data=Skarn, aes(alpha=..level.., fill=factor(STYLE)), bins=7)+
      geom_point(data=Unknown, colour="black")
      
w <- 6
ggsave("Figures/beechworth.png", p, width=w, height=w*0.9)
print(p)