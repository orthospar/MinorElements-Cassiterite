setwd("C:/Users/Jason/OneDrive - research.uwa.edu.au/Analysis/EPMA")
require(ggplot2)
require(dplyr)

source("coloursafe.R") #load colour schemes
source("C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/EPMA Transects/import.R") #load the data
Pegs <- filter(data, STYLE=="Pegmatite")
Vein <- filter(data, STYLE=="Vein")
Skarn <- filter(data, STYLE=="Skarn")
Greisen <- filter(data, STYLE=="Greisen"&LOCALITY!="Blue Tier")
BlueTier <- filter(data, LOCALITY=="Blue Tier")

#selecting data only above 0...
fe.data <- filter(data, Fe.AT.>0)
nb.data <- filter(data, Nb.AT.>0)
ta.data <- filter(data, Ta.AT.>0)
mn.data <- filter(data, Mn.AT.>0)

#...to calculate the mean 99% confidence limit of detection
cdl_Fe <- mean((fe.data$Fe.AT./fe.data$Fe.WT.)*fe.data$Fe.CDL99)
cdl_Nb <- mean((nb.data$Nb.AT./nb.data$Nb.WT.)*nb.data$Nb.CDL99)
cdl_Ta <- mean((ta.data$Ta.AT./ta.data$Ta.WT.)*ta.data$Ta.CDL99)
cdl_Mn <- mean((mn.data$Mn.AT./mn.data$Mn.WT.)*mn.data$Mn.CDL99)

#Build the base plot
p<-ggplot(data=data, aes(x=(Fe.AT.+Mn.AT.), y=(Nb.AT.+Ta.AT.), colour=LOCALITY))+
  scale_colour_manual(values = coloursafe)+
  guides(color = guide_legend(override.aes = list(size=4)))+
  annotate("segment", #plotting the trendlines
           x=c(0,0),xend=c(1,1),
           y=c(0,0),yend=c(1,2),
           colour="grey50", linetype=1)+
  geom_hline(aes(yintercept=cdl_Ta), linetype=2, colour="grey50")+ #plotting the LOD
  geom_vline(aes(xintercept=cdl_Mn), linetype=2, colour="grey50")+ #plotting the LOD
  theme_classic()+
  theme(legend.justification=c(1,1),
        legend.position=c(1,0.5),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "grey90"),
        plot.background = element_rect(fill = "transparent"),
        title = element_text(size=16),
        legend.text= element_text(size=14),
        axis.text = element_text(size=12)
        )+
  coord_fixed(xlim=c(0,1), ylim=c(0,1))+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  labs(x="Fe+Mn (at%)", y="Nb+Ta (at%)", colour=NULL)

#Add data layers to each sub-plot
p1 <- p + geom_point(data=Pegs)+labs(title="A): Pegmatite and Granite-hosted")
p2 <- p + geom_point(data=Vein)+labs(title="C): Vein-hosted")
p3 <- p + geom_point(data=Skarn)+labs(title="D): Skarn")
p4 <- p + geom_point(data=BlueTier)+
          geom_point(data=Greisen)+
          labs(title="B): Greisen")
w <- 6
#save as pdfs
ggsave("Figures/colsub_pegs.pdf", p1, width=w, height=w*0.99)
ggsave("Figures/colsub_vein.pdf", p2, width=w, height=w*0.99)
ggsave("Figures/colsub_skarn.pdf", p3, width=w, height=w*0.99)
ggsave("Figures/colsub_greisen.pdf", p4, width=w, height=w*0.99)