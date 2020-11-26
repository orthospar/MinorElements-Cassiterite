setwd("C:/Users/Jason/OneDrive - research.uwa.edu.au/Analysis/EPMA")
require(dplyr)
require(ggplot2)
require(ggtern)

source("coloursafe.R") #load colour scheme
source("C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/EPMA Transects/import.R") #load data

#order the data so that analyses with higher concentrations of substituting elements plot on top
#(and not buried underneath 'noisy' data)
data <- arrange(data, desc(Sn.OXMOL.))

#subset the data into mineral systems (the STYLE column)
list2env(split.data.frame(data, data$STYLE), envir=.GlobalEnv)

#build the base plot
p<-ggtern(data=NULL, aes(x=Ti.AT., y=(Fe.AT.+Mn.AT.), z=(Ta.AT.+Nb.AT.), colour=Sn.OXMOL.))+
  scale_color_gradientn(colours = coloursafe_grad, na.value="#e5e5e5", limits=c(95,100),
                        breaks = c(95,96,97,98,99,100),
      guide=guide_colourbar(direction ="vertical", label.position = "right", title.position= "top",
                         title.hjust = 0.5, label.vjust = 1, label.hjust = 0.5))+
  annotate("segment", x=c(1,1), xend=c(0.0,0.00),
                      y=c(0,0), yend=c(0.5,0.33),
                      z=c(0,0), zend=c(0.5,0.66),
                      colour="grey50", linetype=1)+
  theme_void()+
  theme_nolabels()+
  theme(panel.background = element_rect(fill = "grey90"),
        plot.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        title = element_text(size=16),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10, angle=0),
        legend.justification =c(0,1),
        legend.position = c(0.08,0.95),
        legend.key.size = unit(10, "mm")
        )+
  labs(x="", y="", z="", colour=expression("SnO"[2]~"(mol%)"))

#add data layers to the sub plots
p.vein <- p + geom_point(data=Vein) + labs(title="C): Vein-hosted")
p.peg <- p + geom_point(data=Pegmatite) + labs(title="A): Pegmatite and Granite-hosted")
p.skarn <- p + geom_point(data=Skarn) + labs(title="D): Skarn")
p.greisen <- p + geom_point(data=Greisen) + labs(title="B): Greisen")

w <- 6
#export the figures
ggsave("Figures/veins.pdf", p.vein, width=w, height=w*0.9)
ggsave("Figures/peg.pdf", p.peg, width=w, height=w*0.9)
ggsave("Figures/skarn.pdf", p.skarn, width=w, height=w*0.9)
ggsave("Figures/greisen.pdf", p.greisen, width=w, height=w*0.9)