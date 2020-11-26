setwd("C:/Users/Jason/OneDrive - research.uwa.edu.au/Analysis/EPMA")
require(ggplot2)
require(dplyr)

source("C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/EPMA Transects/import.R") #load data
source("coloursafe.R") #load colour scheme

tung <- filter(data, W.AT.>0) #filter to only include analyses of W above the LLD
Pegs <- filter(tung, STYLE=="Pegmatite") #subset into mineral systems
Vein <- filter(tung, STYLE=="Vein")
Skarn <- filter(tung, STYLE=="Skarn")
Greisen <- filter(tung, STYLE=="Greisen"&LOCALITY!="Blue Tier")
BlueTier <- filter(tung, LOCALITY=="Blue Tier") #keep Blue Tier separate because it is large

fe.data <- filter(data, Fe.AT.>0) #remove values where Fe is below the LLD...
cdl_Fe <- mean((fe.data$Fe.AT./fe.data$Fe.WT.)*fe.data$Fe.CDL99)#...to calculate the mean LLD
cdl_W <- mean((tung$W.AT./tung$W.WT.)*tung$W.CDL99)#also do for W

#produce the base plot
p <- ggplot(data=tung, aes(x=Fe.AT.+Mn.AT., y=W.AT., colour=LOCALITY))+
  geom_hline(aes(yintercept=cdl_W), linetype=2, colour="grey50")+ #plotting the LLD
  geom_vline(aes(xintercept=cdl_Fe), linetype=2, colour="grey50")+ #plotting the LLD
  scale_colour_manual(values = coloursafe)+
  guides(color = guide_legend(override.aes = list(size=4)))+
  annotate("segment", #plotting trendlines 
           x=c(0,0,0),xend=c(1,1,2),
           y=c(0,0,0),yend=c(1,2,1),
           colour="grey50", linetype=1)+
  theme_classic()+
  theme(legend.justification=c(0,1),
        legend.position=c(0,1),
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
  labs(x="Fe+Mn (at%)", y="W (at%)", colour=NULL)

#add data layers to each plot
p1 <- p + geom_point(data=Pegs)+
          labs(title="A): Pegmatite and Granite-hosted")
p2 <- p + geom_point(data=Vein)+
          labs(title="C): Vein-hosted")
p3 <- p + geom_point(data=Skarn)+
          labs(title="D): Skarn")
p4 <- p + geom_point(data=BlueTier)+
          geom_point(data=Greisen)+
          labs(title="B): Greisen")

w <- 6
#save to pdf
ggsave("Figures/tungsub_pegs.pdf", p1, width=w, height=w*0.99)
ggsave("Figures/tungsub_vein.pdf", p2, width=w, height=w*0.99)
ggsave("Figures/tungsub_skarn.pdf", p3, width=w, height=w*0.99)
ggsave("Figures/tungsub_greisen.pdf", p4, width=w, height=w*0.99)