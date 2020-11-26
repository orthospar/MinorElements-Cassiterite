require(ggplot2)
require(dplyr)

source("coloursafe.R") #load colour scheme
source("C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/EPMA Transects/import.R") #load data


data<-data[(data$Mn.AT.+data$Fe.AT.)>0,] #Filter to Fe+Mn >0
data<-data[(data$Ta.AT.+data$Nb.AT.)>0.05,] #Filter to >0.05 (otherwise too much noise along x-axis)
Pegs <- filter(data, STYLE=="Pegmatite")
Bluetier <- filter(data, LOCALITY=="Blue Tier")
Vein <- filter(data, STYLE=="Vein")

p <- ggplot(data=NULL, aes(x=Mn.AT./(Fe.AT.+Mn.AT.),
                               y=Ta.AT./(Ta.AT.+Nb.AT.),
                               colour=LOCALITY,
                               alpha=(Ta.AT.+Nb.AT.+Fe.AT.+Mn.AT.)))+
  scale_colour_manual(values = coloursafe)+
  scale_alpha_continuous(range=c(0.1,1), limits=c(0,0.3), guide="none")+
  guides(color = guide_legend(override.aes = list(size=4)))+
  coord_fixed(xlim=c(0,1), ylim=c(0,1))+
  scale_x_continuous(expand = c(0.01, 0))+
  scale_y_continuous(expand = c(0.01, 0))+
  theme_classic()+
  theme(
    legend.justification=c(1,0),
    legend.position=c(1,0),
    legend.background = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    title = element_text(size=16),
    legend.text= element_text(size=14),
    axis.text = element_text(size=12),
    legend.key = element_rect(fill = "transparent", colour = NA) # get rid of key legend fill, and of the surrounding
  )+ 
  labs(colour=NULL, alpha=NULL, x="Mn/(Mn+Fe)", y="Ta/(Ta+Nb)", title=NULL)

p1 <- p + geom_point(data=Pegs)+labs(title="A): Pegmatite and Granite-hosted")
p2 <- p + geom_point(data=Bluetier) + geom_point(data=Vein)+labs(title="B): Hybrid and Aqueous Fluid Systems")

w <- 6
ggsave("Figures/colquad_pegs.pdf", p1, width=w, height=w*0.99)
ggsave("Figures/colquad_other.pdf", p2, width=w, height=w*0.99)