setwd("C:/Users/Jason/OneDrive - research.uwa.edu.au/Analysis/Thermodynamics")
library(openxlsx)
library(ggplot2)

S<-read.xlsx("thermodynamics.xlsx", sheet="S") #Standard Entropy table, in J mol-1 K-1
H<-read.xlsx("thermodynamics.xlsx", sheet="dH")  #Standard Enthalpy of Formation table, in kJ mol-1 K-1
H[,-1]<-H[,-1]*1000 #convert from kJ to J
R<-8.3144589 #Gas constant, J mol-1 K-1

#Sn as Sn(OH)x or Sn(Cl)x species?

#Sn(OH)2 + O2 = SnO2 + H2O
#WO2 + H2O + O2 = WO4(OH)2

T_i <- which(S$TK==800)
fO2_NNO <- ((((2*H$NiO)-(2*H$Ni+H$O2))-S$TK*((2*S$NiO)-(2*S$Ni+S$O2)))/(R*S$TK*log(10)))[T_i]
dNNO_QIF<- ((((H$fayalite)-(H$quartz+2*H$Fe+H$O2))-S$TK*((S$fayalite)-(S$quartz+2*S$Fe+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_QFM<- ((((2*H$magnetite+3*H$quartz)-(3*H$fayalite+H$O))-S$TK*((2*S$magnetite+3*S$quartz)-(3*S$fayalite+S$O)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_MH <- ((((6*H$hematite)-(4*H$magnetite+H$O2))-S$TK*((6*S$hematite)-(4*S$magnetite+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_WM <- ((((2*H$magnetite)-(6*H$FeO+H$O2))-S$TK*((2*S$magnetite)-(6*S$FeO+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_CHO<- ((((0.5*H$CO2+H$H2O)-(0.5*H$CH4+H$O2))-S$TK*((0.5*S$CO2+S$H2O)-(0.5*S$CH4+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_IW <- ((((2*H$FeO)-(2*H$Fe+H$O2))-S$TK*((2*S$FeO)-(2*S$Fe+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_WH <- ((((2*H$hematite)-(4*H$FeO+H$O2))-S$TK*((2*S$hematite)-(4*S$FeO+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_H2O<- ((((2*H$H2O)-(2*H$H2+H$O2))-S$TK*((2*S$H2O)-(2*S$H2+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO

dNNO_Nb4_5 <- ((((2*H$Nb2O5)-(4*H$NbO2+H$O2))-S$TK*((2*S$Nb2O5)-(4*S$NbO2+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_Ta0_5 <- (((((2/5)*H$Ta2O5)-((4/5)*H$Ta+H$O2))-S$TK*(((2/5)*S$Ta2O5)-((4/5)*S$Ta+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_SnSnO2 <- ((((H$SnO2)-(H$Sn+H$O2))-S$TK*((S$SnO2)-(S$Sn+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_W4_6 <- ((((2*H$WO3)-(2*H$WO2+H$O2))-S$TK*((2*S$WO3)-(2*S$WO2+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_Mo4_6 <- ((((2*H$MoO3)-(2*H$MoO2+H$O2))-S$TK*((2*S$MoO3)-(2*S$MoO2+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_Ti3_4 <- ((((4*H$TiO2)-(2*H$Ti2O3+H$O2))-S$TK*((4*S$TiO2)-(2*S$Ti2O3+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_Mn3_4 <- ((((4*H$MnO2)-(2*H$Mn2O3+H$O2))-S$TK*((4*S$MnO2)-(2*S$Mn2O3+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_Mn2_3 <- ((((2*H$Mn2O3)-(4*H$MnO+H$O2))-S$TK*((2*S$Mn2O3)-(4*S$MnO+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_Mn0_2 <- ((((2*H$MnO)-(2*H$Mn+H$O2))-S$TK*((2*S$MnO)-(2*S$Mn+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO

dNNO_Pb0_2 <- ((((2*H$PbO)-(2*H$Pb+H$O2))-S$TK*((2*S$PbO)-(2*S$Pb+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_Pb2_4 <- ((((2*H$PbO2)-(2*H$PbO+H$O2))-S$TK*((2*S$PbO2)-(2*S$PbO+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO

dNNO_V0_2 <- ((((2*H$VO)-(2*H$V+H$O2))-S$TK*((2*S$VO)-(2*S$V+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_V2_3 <- ((((2*H$V2O3)-(4*H$VO+H$O2))-S$TK*((2*S$V2O3)-(4*S$VO+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_V3_4 <- ((((4*H$VO2)-(2*H$V2O3+H$O2))-S$TK*((4*S$VO2)-(2*S$V2O3+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_V4_5 <- ((((2*H$V2O5)-(4*H$VO2+H$O2))-S$TK*((2*S$V2O5)-(4*S$VO2+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO

dNNO_Cr0_2 <- ((((2*H$CrO)-(2*H$Cr+H$O2))-S$TK*((2*S$CrO)-(2*S$Cr+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_Cr2_3 <- ((((2*H$Cr2O3)-(4*H$CrO+H$O2))-S$TK*((2*S$Cr2O3)-(4*S$CrO+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_Cr3_4 <- ((((4*H$CrO2)-(2*H$Cr2O3+H$O2))-S$TK*((4*S$CrO2)-(2*S$Cr2O3+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
dNNO_Cr4_6 <- ((((2*H$CrO3)-(2*H$CrO2+H$O2))-S$TK*((2*S$CrO3)-(2*S$CrO2+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO

k_Nb <- 10^((dNNO_Nb4_5*1)/-4)
k_Ta <- 10^((dNNO_Ta0_5*5)/-4)
k_W  <- 10^((dNNO_W4_6*2)/-4)
k_Sn <- 10^((dNNO_SnSnO2*4)/-4)
k_Mo <- 10^((dNNO_Mo4_6*2)/-4)
k_Ti <- 10^((dNNO_Ti3_4*1)/-4)
k_Mn4<- 10^((dNNO_Mn3_4*1)/-4)
k_Mn3<- 10^((dNNO_Mn2_3*1)/-4)
k_Mn2<- 10^((dNNO_Mn0_2*2)/-4)
k_Fe2<- 10^((dNNO_IW*2/-4))
k_Fe3<- 10^((dNNO_WH*1/-4))

k_Pb2<- 10^((dNNO_Pb0_2*2)/-4)
k_Pb4<- 10^((dNNO_Pb2_4*2)/-4)

k_V5 <- 10^((dNNO_V4_5*1)/-4)
k_V4 <- 10^((dNNO_V3_4*1)/-4)
k_V3 <- 10^((dNNO_V2_3*1)/-4)
k_V2 <- 10^((dNNO_V0_2*2)/-4)

k_Cr6<- 10^((dNNO_Cr4_6*2)/-4)
k_Cr4<- 10^((dNNO_Cr3_4*1)/-4)
k_Cr3<- 10^((dNNO_Cr2_3*1)/-4)
k_Cr2<- 10^((dNNO_Cr0_2*2)/-4)

dNNO <- seq(-50,30,0.1)
R_Nb <- (k_Nb*10^((dNNO*1)/4))/(1+(k_Nb*10^((dNNO*1)/4)))
R_Ta <- (k_Ta*10^((dNNO*5)/4))/(1+(k_Ta*10^((dNNO*5)/4)))
R_W  <- (k_W*10^((dNNO*2)/4))/(1+(k_W*10^((dNNO*2)/4)))
R_Mo  <- (k_Mo*10^((dNNO*2)/4))/(1+(k_Mo*10^((dNNO*2)/4)))
R_Sn <- (k_Sn*10^((dNNO*4)/4))/(1+(k_Sn*10^((dNNO*4)/4)))
R_Ti <- (k_Ti*10^((dNNO*1)/4))/(1+(k_Ti*10^((dNNO*1)/4)))
R_Mn4 <- (k_Mn4*10^((dNNO*1)/4))/(1+(k_Mn4*10^((dNNO*1)/4)))
R_Mn3 <- (k_Mn3*10^((dNNO*1)/4))/(1+(k_Mn3*10^((dNNO*1)/4)))
R_Mn2 <- (k_Mn2*10^((dNNO*2)/4))/(1+(k_Mn2*10^((dNNO*2)/4)))
R_Mn2t<- R_Mn2-(R_Mn3+R_Mn4)
R_Mn3t<- R_Mn3-(R_Mn4)
R_Fe3 <- (k_Fe3*10^((dNNO*1)/4))/(1+(k_Fe3*10^((dNNO*1)/4)))
R_Fe2 <- (k_Fe2*10^((dNNO*2)/4))/(1+(k_Fe2*10^((dNNO*2)/4)))
R_Fe2t<- R_Fe2-R_Fe3
R_Fe0t<- 1-(R_Fe2+R_Fe3)

R_Pb4 <- (k_Pb4*10^((dNNO*1)/4))/(1+(k_Pb4*10^((dNNO*1)/4)))
R_Pb2 <- (k_Pb2*10^((dNNO*2)/4))/(1+(k_Pb2*10^((dNNO*2)/4)))

R_V5 <- (k_V5*10^((dNNO*1)/4))/(1+(k_V5*10^((dNNO*1)/4)))
R_V4 <- (k_V4*10^((dNNO*1)/4))/(1+(k_V4*10^((dNNO*1)/4)))
R_V3 <- (k_V3*10^((dNNO*1)/4))/(1+(k_V3*10^((dNNO*1)/4)))
R_V2 <- (k_V2*10^((dNNO*2)/4))/(1+(k_V2*10^((dNNO*2)/4)))

R_Cr6 <- (k_Cr6*10^((dNNO*2)/4))/(1+(k_Cr6*10^((dNNO*2)/4)))
R_Cr4 <- (k_Cr4*10^((dNNO*1)/4))/(1+(k_Cr4*10^((dNNO*1)/4)))
R_Cr3 <- (k_Cr3*10^((dNNO*1)/4))/(1+(k_Cr3*10^((dNNO*1)/4)))
R_Cr2 <- (k_Cr2*10^((dNNO*2)/4))/(1+(k_Cr2*10^((dNNO*2)/4)))

speciation <- data.frame(dNNO, R_Nb, R_W, R_Sn)

plot <- ggplot(speciation, aes(x=dNNO))+
  annotate(geom="rect", xmin=-5, xmax=-2, ymin=0, ymax=1, fill="grey80")+
  geom_vline(xintercept = dNNO_MH, colour="red", linetype=2)+
  geom_vline(xintercept = dNNO_QFM, colour="#33a02c", linetype=2)+
  geom_vline(xintercept = dNNO_WM, colour="#1f78b4", linetype=2)+
  geom_vline(xintercept = dNNO_QIF, colour="black", linetype=2)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.title.y = element_text(angle = 0, vjust = 0.5))+
  coord_cartesian(xlim=c(-25,25), ylim=c(0,1.01), expand=FALSE)+
  labs(x=expression("log"[10]~italic("f")[O[2]]~(Delta*"NNO")), y=expression(over(" M"^"n+",Sigma*"M")))

plotTi <- plot +
  geom_line(aes(y=R_Ti), colour="black")+
  geom_line(aes(y=R_Nb), colour="hot pink")+
  geom_line(aes(y=R_Ta), colour="#c73794")+
  geom_line(aes(y=R_W), colour="#204080")

plotMn <- plot +
  geom_line(aes(y=R_Mn2t), colour="black")+
  geom_line(aes(y=R_Mn3t), colour="grey40")+
  geom_line(aes(y=R_Mn4), colour="grey60")

plotFe <- plot+
  #geom_vline(xintercept = dNNO_CHO, colour="grey50", linetype=2)+
  #geom_vline(xintercept = dNNO_H2O, colour="blue", linetype=2)+
  geom_line(aes(y=R_Fe0t), colour="black")+
  geom_line(aes(y=R_Fe2t), colour="grey40")+
  geom_line(aes(y=R_Fe3), colour="grey60")+
  geom_line(aes(y=R_Sn), colour="purple")

plotPb <- plot+
  geom_line(aes(y=R_Pb2), colour="black")

plotV <- plot+
  geom_line(aes(y=R_V5), colour="black")+
  geom_line(aes(y=R_V4), colour="blue")+
  geom_line(aes(y=R_V3), colour="green")+
  geom_line(aes(y=R_V2), colour="red")+
  coord_cartesian(xlim=c(-25,50), ylim=c(0,1.01), expand=FALSE)

plotCr <- plot+
  geom_line(aes(y=R_Cr6), colour="blue")+
  geom_line(aes(y=R_Cr4), colour="black")+
  geom_line(aes(y=R_Cr3), colour="green")+
  geom_line(aes(y=R_Cr2), colour="red")

print(plotV)
#print(plotCr)
#plot to show expected fO2 region, based on Sn-Sno2 and Sn-Cl + Sn-OH reactions
#i.e., what seems to be thermodynamically possible...min and max?
#at CH4:CO2, mole fraction of Nb4+ is:
#1-(k_Nb*10^((dNNO_CHO*1)/4))/(1+(k_Nb*10^((dNNO_CHO*1)/4)))
#0.02156166, ~2.16%
#at Sn:SnO2, mole fraction of Nb4+ is:
#1-(k_Nb*10^((dNNO_SnSnO2*1)/4))/(1+(k_Nb*10^((dNNO_SnSnO2*1)/4)))
#0.03011573, ~3.01%
#0.01774869 @ NNO-4, ~1.77%
#0.03113209 @ NNO-5, ~3.11% *****Most likely lower limit*****
#0.0540519 @ NNO-6, ~5.41%
#0.09223912 @ NNO-7, ~9.22%
#calculate KDs for Sn-granite...then model Nb4+ concentration in cassiterite versus fO2
#assume melt saturated in Sn and Nb (so Sn concentration and Nb concentration are known)
#Ooh, do this for Ti, etc as well...so both a magmatic and an aqueous model are presented


#print(plotTi)
#print(plotMn)
#print(plotFe)

w<-6
#ggsave("Mn.pdf", plotMn, width=w, height=w/sqrt(2)) 
#ggsave("Fe.pdf", plotFe, width=w, height=w/sqrt(2))
#ggsave("Ti.pdf", plotTi, width=w, height=w/sqrt(2))
