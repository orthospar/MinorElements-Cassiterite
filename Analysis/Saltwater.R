require(tidyverse)
require(Cairo)

("C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/EPMA Transects/import.R") #load the data
source("coloursafe.R") #set the colour scale


source("C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/EPMA Transects/import.R") #load the data
#Filter data to analyses from Saltwater Creek with W above the LLD
Saltwater <- dplyr::filter(data, LOCALITY=="Saltwater Creek" & W.AT.>0)

#Calculate the net charge (should sum to 0)
Saltwater$Charge <- Saltwater$Sn.AT.*4 +Saltwater$Ti.AT.*4 +Saltwater$Fe.AT.*2 +Saltwater$Nb.AT.*5 +Saltwater$Ta.AT.*5 +Saltwater$Mn.AT.*2 +Saltwater$Zr.AT.*4-100/3*4

#the calculated fraction of W4+  
f <- 0.25

p <- ggplot(data=Saltwater, aes(y=W.AT.))+
  scale_colour_manual(values = coloursafe,
                      labels=c(expression("100% W"^{"4+"}),
                               expression("100% W"^{"6+"}),
                               expression("25% W"^{"4+"})))+
  guides(color = guide_legend(override.aes = list(size=4), label.position = "left",
                              label.vjust = 1, label.hjust = 1))+
  annotate("segment",
           x=c(0),xend=c(0),
           y=c(0),yend=c(0.3),
           colour="grey50", linetype=2)+
  geom_point(aes(x=Charge+(W.AT.*6), colour="100% W6+"))+ #assuming all W6+ expression()
  geom_point(aes(x=Charge+(W.AT.*4), colour="100% W4+"))+ #assuming all W4+
  geom_point(aes(x=Charge+(W.AT.*6)*(1-f)+(W.AT.*4)*f, colour="25% W4+"))+ #assuming 'f' fraction of W4+
  theme_classic()+
  theme(legend.justification=c(1,1),
        legend.position=c(1,1),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "grey90"),
        plot.background = element_rect(fill = "transparent"),
        legend.text= element_text(size = 14),
        title = element_text(size = 16)
  )+
  scale_x_continuous(limits=c(-0.5,0.5))+
  scale_y_continuous(limits=c(0, 0.3))+
  labs(x="Net Charge", y="W (at%)", colour=NULL)
  
print(p)
w <- 5
ggsave("Figures/charge_saltwater.pdf", p, width=w, height=w/sqrt(2))