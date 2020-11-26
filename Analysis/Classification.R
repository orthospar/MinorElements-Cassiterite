require(ggplot2)
require(ggtern)
require(dplyr)

source("coloursafe.R") #load colour scheme
source("C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/EPMA Transects/import.R") #load data


#remove beechworth from the data for separate plotting
Beech <- filter(data, LOCALITY=="Beechworth")
data <- filter(data, LOCALITY!="Beechworth")

p <- ggtern(data=data, aes(x=Ti.AT., y=(Fe.AT.+Mn.AT.), z=(Ta.AT.+Nb.AT.)))+
  scale_fill_manual(values = coloursafe)+
  scale_color_gradientn(colours = coloursafe_grad, na.value="#e5e5e5", limits=c(94,100),
                        breaks = c(94,96,98,100),
                        guide=guide_colourbar(direction ="horizontal",title.position= "top",
                                              title.hjust = 0.5, label.vjust = 1, label.hjust = 0.5))+
  scale_alpha(range=c(0.1,0.5), guide="none")+
  annotate("segment", x=c(1,1), xend=c(0.0,0.00),
           y=c(0,0), yend=c(0.5,0.33),
           z=c(0,0), zend=c(0.5,0.66),
           colour="grey50", linetype=1)+
  theme_void()+
  theme_nolabels()+
  stat_density_tern(geom='polygon', bins=5,
                    aes(fill=factor(STYLE, levels=c("Pegmatite", "Greisen", "Vein", "Skarn", "Unknown")),
                        #weight=100-Sn.AT.,
                        alpha=..level..))+
  geom_point(data=Beech, aes(colour=Sn.OXMOL.))+
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
  labs(x="", y="", z="", fill="Mineral System", colour=expression("SnO"[2]~"(mol%)"))

print(p)

w<-6
ggsave("Figures/Beechworth_classification.pdf", p, width=w, height=w*0.9)