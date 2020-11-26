setwd("C:/Users/Jason/OneDrive - research.uwa.edu.au/Analysis/EPMA")
require(ggplot2)
require(Cairo)
source("C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/EPMA Transects/import.R")

n_bdl <- function(x) {return(c(y=0, label=sum(x==0)))}
size <- function(x) {return(c(y=max(x), label=sum(x>0)))}

err <- signif(mean((data$Ta..ERR*data$Ta.AT.)/100),digits=1)
l <- paste0("Ti (at%), ",sprintf('\u03C3'),": ",err)
Ti<-ggplot(data=data, aes(x=LOCALITY, y=Ti.AT.))+
  geom_violin(scale="width", draw_quantiles=c(0.5), colour="#1f78b4", fill="#a6cee3")+
  stat_summary(fun.y ="mean", geom="point", shape=21, colour="black", fill="#fc8d62")+
  stat_summary(fun.data ="size", geom="text", size=3, colour="#1f78b4", hjust=0, position=position_nudge(y=+0.015))+
  stat_summary(fun.data ="n_bdl", geom="text", size=3, colour="grey50", hjust=1, position=position_nudge(y=-0.015))+
  stat_summary(aes(y=((Ti.AT./Ti.WT.)*Ti.CDL99)), fun.y ="mean", geom="point", colour="grey50", shape=124, size=5)+
  #geom_linerange(aes(ymin=1-mean((Ti..ERR*Ti.AT.)/100), ymax=1+mean((Ti..ERR*Ti.AT.)/100)), colour="grey50")+
  facet_grid(rows=vars(STYLE), scales="free", space="free", switch="y")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "grey90"),
        plot.background = element_rect(fill = "transparent"),
        strip.text.y = element_blank())+
  scale_y_continuous(limits=c(-0.07,1.5), sec.axis = sec_axis(trans=~./(1/3), name = expression(paste(italic("apfu"),"*100"))))+
  coord_flip()+
  labs(y=l, x=NULL)

err <- signif(mean((data$Fe..ERR*data$Fe.AT.)/100),digits=1)
l <- paste0("Fe (at%), ",sprintf('\u03C3'),": ",err)
Fe<-ggplot(data=data, aes(x=LOCALITY, y=Fe.AT.))+
  geom_violin(scale="width", draw_quantiles=c(0.5), colour="#1f78b4", fill="#a6cee3")+
  stat_summary(fun.y ="mean", geom="point", shape=21, colour="black", fill="#fc8d62")+
  stat_summary(fun.data ="size", geom="text", size=3, colour="#1f78b4", hjust=0, position=position_nudge(y=+0.015))+
  stat_summary(fun.data ="n_bdl", geom="text", size=3, colour="grey50", hjust=1, position=position_nudge(y=-0.015))+
  stat_summary(aes(y=((Fe.AT./Fe.WT.)*Fe.CDL99)), fun.y ="mean", geom="point", colour="grey50", shape=124, size=5)+
  #geom_linerange(aes(ymin=1-mean((Fe..ERR*Fe.AT.)/100), ymax=1+mean((Fe..ERR*Fe.AT.)/100)), colour="grey50")+
  facet_grid(rows=vars(STYLE), scales="free", space="free", switch="y")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "grey90"),
        plot.background = element_rect(fill = "transparent"),
        strip.text.y = element_blank())+
  scale_y_continuous(limits=c(-0.07,1.5), sec.axis = sec_axis(trans=~./(1/3), name = expression(paste(italic("apfu"),"*100"))))+
  coord_flip()+
  labs(y=l, x=NULL)

err <- signif(mean((data$Nb..ERR*data$Nb.AT.)/100),digits=1)
l <- paste0("Nb (at%), ",sprintf('\u03C3'),": ",err)
Nb<-ggplot(data=data, aes(x=LOCALITY, y=Nb.AT.))+
  geom_violin(scale="width", draw_quantiles=c(0.5), colour="#1f78b4", fill="#a6cee3")+
  stat_summary(fun.y ="mean", geom="point", shape=21, colour="black", fill="#fc8d62")+
  stat_summary(fun.data ="size", geom="text", size=3, colour="#1f78b4", hjust=0, position=position_nudge(y=+0.015))+
  stat_summary(fun.data ="n_bdl", geom="text", size=3, colour="grey50", hjust=1, position=position_nudge(y=-0.015))+
  stat_summary(aes(y=((Nb.AT./Nb.WT.)*Nb.CDL99)), fun.y ="mean", geom="point", colour="grey50", shape=124, size=5)+
  #geom_linerange(aes(ymin=1-mean((Nb..ERR*Nb.AT.)/100), ymax=1+mean((Nb..ERR*Nb.AT.)/100)), colour="grey50")+
  facet_grid(rows=vars(STYLE), scales="free", space="free", switch="y")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "grey90"),
        plot.background = element_rect(fill = "transparent"),
        strip.text.y = element_blank())+
  scale_y_continuous(limits=c(-0.07,1.5), sec.axis = sec_axis(trans=~./(1/3), name = expression(paste(italic("apfu"),"*100"))))+
  coord_flip()+
  labs(y=l, x=NULL)

err <- signif(mean((data$Ta..ERR*data$Ta.AT.)/100),digits=1)
l <- paste0("Ta  (at%), ",sprintf('\u03C3'),": ",err)
Ta<-ggplot(data=data, aes(x=LOCALITY, y=Ta.AT.))+
  geom_violin(scale="width", draw_quantiles=c(0.5), colour="#1f78b4", fill="#a6cee3")+
  stat_summary(fun.y ="mean", geom="point", shape=21, colour="black", fill="#fc8d62")+
  stat_summary(fun.data ="size", geom="text", size=3, colour="#1f78b4", hjust=0, position=position_nudge(y=+0.015))+
  stat_summary(fun.data ="n_bdl", geom="text", size=3, colour="grey50", hjust=1, position=position_nudge(y=-0.015))+
  stat_summary(aes(y=((Ta.AT./Ta.WT.)*Ta.CDL99)), fun.y ="mean", geom="point", colour="grey50", shape=124, size=5)+
  #geom_linerange(aes(ymin=1-mean((Ta..ERR*Ta.AT.)/100), ymax=1+mean((Ta..ERR*Ta.AT.)/100)), colour="grey50")+
  facet_grid(rows=vars(STYLE), scales="free", space="free", switch="y")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "grey90"),
        plot.background = element_rect(fill = "transparent"),
        strip.text.y = element_blank())+
  scale_y_continuous(limits=c(-0.07,1.5), sec.axis = sec_axis(trans=~./(1/3), name = expression(paste(italic("apfu"),"*100"))))+
  coord_flip()+
  labs(y=l, x=NULL)

err <- signif(mean((data$W..ERR*data$W.AT.)/100),digits=1)
l <- paste0("W (at%), ",sprintf('\u03C3'),": ",err)
W<-ggplot(data=data, aes(x=LOCALITY, y=W.AT.))+
  geom_violin(scale="width", draw_quantiles=c(0.5), colour="#1f78b4", fill="#a6cee3")+
  stat_summary(fun.y ="mean", geom="point", shape=21, colour="black", fill="#fc8d62")+
  stat_summary(fun.data ="size", geom="text", size=3, colour="#1f78b4", hjust=0, position=position_nudge(y=+0.015))+
  stat_summary(fun.data ="n_bdl", geom="text", size=3, colour="grey50", hjust=1, position=position_nudge(y=-0.015))+
  stat_summary(aes(y=((W.AT./W.WT.)*W.CDL99)), fun.y ="mean", geom="point", colour="grey50", shape=124, size=5)+
  #geom_linerange(aes(ymin=0.6-mean((W..ERR*W.AT.)/100), ymax=0.6+mean((W..ERR*W.AT.)/100)), colour="grey50")+
  facet_grid(rows=vars(STYLE), scales="free", space="free", switch="y")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "grey90"),
        plot.background = element_rect(fill = "transparent"),
        strip.text.y = element_blank())+
  scale_y_continuous(limits=c(-0.04,0.6), sec.axis = sec_axis(trans=~./(1/3), name = expression(paste(italic("apfu"),"*100"))))+
  coord_flip()+
  labs(y=l, x=NULL)

err <- signif(mean((data$Mn..ERR*data$Mn.AT.)/100),digits=1)
l <- paste0("Mn (at%), ",sprintf('\u03C3'),": ",err)
Mn<-ggplot(data=data, aes(x=LOCALITY, y=Mn.AT.))+
  geom_violin(scale="width", draw_quantiles=c(0.5), colour="#1f78b4", fill="#a6cee3")+
  stat_summary(fun.y ="mean", geom="point", shape=21, colour="black", fill="#fc8d62")+
  stat_summary(fun.data ="size", geom="text", size=3, colour="#1f78b4", hjust=0, position=position_nudge(y=+0.0015))+
  stat_summary(fun.data ="n_bdl", geom="text", size=3, colour="grey50", hjust=1, position=position_nudge(y=-0.0015))+
  stat_summary(aes(y=((Mn.AT./Mn.WT.)*Mn.CDL99)), fun.y ="mean", geom="point", colour="grey50", shape=124, size=5)+
  #geom_linerange(aes(ymin=0.2-mean((Mn..ERR*Mn.AT.)/100), ymax=0.2+mean((Mn..ERR*Mn.AT.)/100)), colour="grey50")+
  facet_grid(rows=vars(STYLE), scales="free", space="free", switch="y")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "grey90"),
        plot.background = element_rect(fill = "transparent"),
        strip.text.y = element_blank())+
  scale_y_continuous(limits=c(-0.01,0.25), sec.axis = sec_axis(trans=~./(1/3), name = expression(paste(italic("apfu"),"*100"))))+
  coord_flip()+
  labs(y=l, x=NULL)

err <- signif(mean((data$Zr..ERR*data$Zr.AT.)/100),digits=1)
l <- paste0("Zr (at%), ",sprintf('\u03C3'),": ",err)
Zr<-ggplot(data=data, aes(x=LOCALITY, y=Zr.AT.))+
  geom_violin(scale="width", draw_quantiles=c(0.5), colour="#1f78b4", fill="#a6cee3")+
  stat_summary(fun.y ="mean", geom="point", shape=21, colour="black", fill="#fc8d62")+
  stat_summary(fun.data ="size", geom="text", size=3, colour="#1f78b4", hjust=0, position=position_nudge(y=+0.0015))+
  stat_summary(fun.data ="n_bdl", geom="text", size=3, colour="grey50", hjust=1, position=position_nudge(y=-0.0015))+
  stat_summary(aes(y=((Zr.AT./Zr.WT.)*Zr.CDL99)), fun.y ="mean", geom="point", colour="grey50", shape=124, size=5)+
  #geom_linerange(aes(ymin=0.2-mean((Zr..ERR*Zr.AT.)/100), ymax=0.2+mean((Zr..ERR*Zr.AT.)/100)), colour="grey50")+
  facet_grid(rows=vars(STYLE), scales="free", space="free", switch="y")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "grey90"),
        plot.background = element_rect(fill = "transparent"),
        strip.text.y = element_blank())+
  scale_y_continuous(limits=c(-0.01,0.25), sec.axis = sec_axis(trans=~./(1/3), name = expression(paste(italic("apfu"),"*100"))))+
  coord_flip()+
  labs(y=l, x=NULL)

sizeSn <- function(x) {return(c(y=100/3, label=sum(x>0)))}
err <- signif(mean((data$Sn..ERR*data$Sn.AT.)/100),digits=1)
l <- paste0("Sn (at%), ",sprintf('\u03C3'),": ",err)
Sn<-ggplot(data=data, aes(x=LOCALITY, y=Sn.AT.))+
  geom_violin(scale="width", draw_quantiles=c(0.5), colour="#1f78b4", fill="#a6cee3")+
  stat_summary(fun.y ="mean", geom="point", shape=21, colour="black", fill="#fc8d62")+
  geom_hline(yintercept = 100/3, linetype="dashed")+
  stat_summary(fun.data ="sizeSn", geom="text", size=3, colour="#1f78b4", hjust=0, position=position_nudge(y=+0.015))+
  #geom_linerange(aes(ymin=32-mean((Sn..ERR*Sn.AT.)/100), ymax=32+mean((Sn..ERR*Sn.AT.)/100)), colour="grey50")+
  facet_grid(rows=vars(STYLE), scales="free", space="free", switch="y")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "grey90"),
        plot.background = element_rect(fill = "transparent"),
        strip.text.y = element_blank())+
  scale_y_continuous(limits=c(31.5,33.4), sec.axis = sec_axis(trans=~./(1/3), name = expression(paste(italic("apfu"),"*100"))))+
  coord_flip()+
  labs(y=l, x=NULL)

ggsave("Figures/1violin_Ti.pdf", Ti, width=14, height=14, units="cm", device=cairo_pdf)
ggsave("Figures/1violin_Fe.pdf", Fe, width=14, height=14, units="cm", device=cairo_pdf)
ggsave("Figures/1violin_Nb.pdf", Nb, width=14, height=14, units="cm", device=cairo_pdf)
ggsave("Figures/1violin_Ta.pdf", Ta, width=14, height=14, units="cm", device=cairo_pdf)
ggsave("Figures/1violin_Mn.pdf", Mn, width=14, height=14, units="cm", device=cairo_pdf)
ggsave("Figures/1violin_Zr.pdf", Zr, width=14, height=14, units="cm", device=cairo_pdf)
ggsave("Figures/1violin_W.pdf",   W, width=14, height=14, units="cm", device=cairo_pdf)
ggsave("Figures/1violin_Sn.pdf", Sn, width=14, height=14, units="cm", device=cairo_pdf)