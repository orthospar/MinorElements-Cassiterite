setwd("C:/Users/Jason/OneDrive - research.uwa.edu.au/Analysis/Thermodynamics")
library(ggplot2)
library(openxlsx)
library(ggrepel)
library(scales)
library(dplyr)

source("Model.R")

r <- seq(0.3,1.5,0.01)
curves <- data.frame(radius=r, Di4=Di(r, 4, 800), Di3=Di(r, 3, 800), Di2=Di(r, 2, 800), Di5=Di(r, 5, 800), Di6=Di(r, 6, 800))

ions <- read.xlsx("ionic_radii.xlsx", sheet="minor")
ions <- filter(ions, spin=="H"|is.na(spin)==TRUE)
ions <- cbind(ions, D=Di(ions$radius, ions$charge, TK=800))
ions.labels <- add_row(ions, D=c(curves$Di4, curves$Di3, curves$Di2, curves$Di5, curves$Di6), radius=rep(r, 5), name=rep("", 121*5))

base <- ggplot(curves, aes(x=radius))+
  geom_line(aes(y=log10(Di4)), colour="grey80", linetype=1)+
  geom_line(aes(y=log10(Di3)), colour="grey80", linetype=2)+
  geom_line(aes(y=log10(Di2)), colour="grey80", linetype=3)+
  geom_line(aes(y=log10(Di5)), colour="grey80", linetype=4)+
  geom_line(aes(y=log10(Di6)), colour="grey80", linetype=5)+
  scale_y_continuous(labels=math_format(), breaks=c(-8,-6,-4,-2,0,2))+
  coord_cartesian(ylim=c(-8,+2), xlim=c(0.4,1.2))+
  theme_classic()+
  labs(x=expression("Ionic Radius"~ring("(A)")), y=expression(Partition~Coefficient~bgroup("(",""^{frac(cst,liq)}*D[italic(frac(M^{"n+"},Sn^{"4+"}))],")")))

minor<- base +
  geom_point(data=ions, aes(x=radius, y=log10(D)), colour="dark blue")+
  geom_text_repel(data=ions.labels, aes(x=radius, y=log10(D), label=name), size=3, parse=TRUE,
                  box.padding=0.5, point.padding=0.4, nudge_y=0.2,
                  segment.size=0.2, segment.color="dark blue", segment.alpha="0.5")

ree <- read.xlsx("ionic_radii.xlsx", sheet="trace_REE")
ree <- cbind(ree, D=Di(ree$radius, ree$charge, TK=800))
ree.labels <- add_row(ree, D=c(curves$Di4, curves$Di3, curves$Di2, curves$Di5, curves$Di6), radius=rep(r, 5), name=rep("", 121*5))

earths <- base +
  geom_point(data=ree, aes(x=radius, y=log10(D)), colour="dark blue")+
  geom_text_repel(data=ree.labels, aes(x=radius, y=log10(D), label=name), size=3, parse=TRUE,
                  box.padding=0.5, point.padding=0.4, nudge_y=0.2,
                  segment.size=0.2, segment.color="dark blue", segment.alpha="0.5")

radio <- read.xlsx("ionic_radii.xlsx", sheet="trace_radiogenic")
radio <- cbind(radio, D=Di(radio$radius, radio$charge, TK=800))
radio.labels <- add_row(radio, D=c(curves$Di4, curves$Di3, curves$Di2, curves$Di5, curves$Di6), radius=rep(r, 5), name=rep("", 121*5))

radiogenics <- base +
  geom_point(data=radio, aes(x=radius, y=log10(D)), colour="dark blue")+
  geom_text_repel(data=radio.labels, aes(x=radius, y=log10(D), label=name), size=3, parse=TRUE,
                  box.padding=0.5, point.padding=0.4, nudge_y=0.2,
                  segment.size=0.2, segment.color="dark blue", segment.alpha="0.5")

mix <- read.xlsx("ionic_radii.xlsx", sheet="trace_mix")
mix <- cbind(mix, D=Di(mix$radius, mix$charge, TK=800))
mix.labels <- add_row(mix, D=c(curves$Di4, curves$Di3, curves$Di2, curves$Di5, curves$Di6), radius=rep(r, 5), name=rep("", 121*5))

trace <- base +
  geom_point(data=mix, aes(x=radius, y=log10(D)), colour="dark blue")+
  geom_text_repel(data=mix.labels, aes(x=radius, y=log10(D), label=name), size=3, parse=TRUE,
                  box.padding=0.5, point.padding=0.4, nudge_y=0.2,
                  segment.size=0.2, segment.color="dark blue", segment.alpha="0.5")


coupled_minor <- read.xlsx("ionic_radii.xlsx", sheet="coupled")
coupled_minor <- cbind(coupled_minor, D=Di(coupled_minor$radius, coupled_minor$charge, TK=800))
coupled <- base +
  geom_point(data=ions, aes(x=radius, y=log10(D)), colour="grey50")+
  geom_point(data=coupled_minor, aes(x=radius, y=log10(D)), colour="dark blue")+
  coord_cartesian(ylim=c(-1,+1), xlim=c(0.6,0.8))+
  scale_y_continuous(labels=math_format(), breaks=c(-1,0,1))+
  geom_text_repel(data=coupled_minor, aes(x=radius, y=log10(D), label=name), size=3, parse=TRUE,
                  box.padding=0.8, point.padding=0.4, nudge_y=0.2,
                  segment.size=0.2, segment.color="dark blue", segment.alpha="0.5")
 
w<-6
ggsave("MinorElements.pdf", minor, width=w, height=w/sqrt(2))
ggsave("RareEarths.png", earths, width=w, height=w/sqrt(2))
ggsave("Radiogenics.png", radiogenics, width=w, height=w/sqrt(2))
ggsave("TraceElements.png", trace, width=w, height=w/sqrt(2))
ggsave("CoupledSubstitutions.pdf", coupled, width=w, height=w/sqrt(2))