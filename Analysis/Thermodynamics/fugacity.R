setwd("C:/Users/Jason/OneDrive - research.uwa.edu.au/Analysis/Thermodynamics")

library(openxlsx)
library(ggplot2)

S<-read.xlsx("thermodynamics.xlsx", sheet="S") #Standard Entropy table, in J mol-1 K-1
H<-read.xlsx("thermodynamics.xlsx", sheet="dH")  #Standard Enthalpy of Formation table, in kJ mol-1 K-1
H[,-1]<-H[,-1]*1000 #convert from kJ to J
R<-8.3144589 #Gas constant, J mol-1 K-1

#fugacity at equilibrium = (dH - TdS)/RTln(10)

#FMQ: 3(Fe2SiO4) + O2 = 2(Fe3O4) + 3(SiO2)
FMQ <- (((2*H$magnetite+3*H$quartz)-(3*H$fayalite+H$O))-S$TK*((2*S$magnetite+3*S$quartz)-(3*S$fayalite+S$O)))/(R*S$TK*log(10))

#WM: 6(FeO) + O2 = 2(Fe2O3)
WM <- (((2*H$magnetite)-(6*H$FeO+H$O2))-S$TK*((2*S$magnetite)-(6*S$FeO+S$O2)))/(R*S$TK*log(10))

#IW: 2(Fe) + O2 = 2(Fe2O3)
IW <- (((2*H$FeO)-(2*H$Fe+H$O2))-S$TK*((2*S$FeO)-(2*S$Fe+S$O2)))/(R*S$TK*log(10))

#NNO: 2(Ni) + O2 = 2(NiO)
NNO <- (((2*H$NiO)-(2*H$Ni+H$O2))-S$TK*((2*S$NiO)-(2*S$Ni+S$O2)))/(R*S$TK*log(10))

#MH: 4(Fe3O4) + O2 = 6(Fe2O3)
MH <- (((6*H$hematite)-(4*H$magnetite+H$O2))-S$TK*((6*S$hematite)-(4*S$magnetite+S$O2)))/(R*S$TK*log(10))

#QIF: SiO2 + 2(Fe) + O2 = Fe2SiO4
QIF <- (((H$fayalite)-(H$quartz+2*H$Fe+H$O2))-S$TK*((S$fayalite)-(S$quartz+2*S$Fe+S$O2)))/(R*S$TK*log(10))

#NbO2/Nb2O5: 4(NbO2) + O2 = 2(Nb2O5)
Nb4_5 <- (((2*H$Nb2O5)-(4*H$NbO2+H$O2))-S$TK*((2*S$Nb2O5)-(4*S$NbO2+S$O2)))/(R*S$TK*log(10))
Ta0_5 <- ((((2/5)*H$Ta2O5)-((4/5)*H$Ta+H$O2))-S$TK*(((2/5)*S$Ta2O5)-((4/5)*S$Ta+S$O2)))/(R*S$TK*log(10))


#Sn/SnO2: Sn + O2 = SnO2
SnSnO2 <- (((H$SnO2)-(H$Sn+H$O2))-S$TK*((S$SnO2)-(S$Sn+S$O2)))/(R*S$TK*log(10))

#WO2/WO3: 2(WO2) + O2 = 2(WO3)
W4_6 <- (((2*H$WO3)-(2*H$WO2+H$O2))-S$TK*((2*S$WO3)-(2*S$WO2+S$O2)))/(R*S$TK*log(10))

CHO <- (((0.5*H$CO2+H$H2O)-(0.5*H$CH4+H$O2))-S$TK*((0.5*S$CO2+S$H2O)-(0.5*S$CH4+S$O2)))/(R*S$TK*log(10))
H2O <- (((2*H$H2O)-(2*H$H2+H$O2))-S$TK*((2*S$H2O)-(2*S$H2+S$O2)))/(R*S$TK*log(10))


fugacity <- data.frame(cbind(TK=S$TK,FMQ,WM,IW,NNO,MH,QIF,Nb4_5,SnSnO2,W4_6))

buffers <- ggplot(fugacity, aes(x=TK))+
  annotate(geom="polygon", x=c(700,700,750,800,825,825,800,750), y=c(-31.2,-26.1,-24,-21.8,-21.1,-25.8,-26.8,-28.8), fill="grey80")+
  geom_line(aes(y=FMQ), colour="#33a02c", size=0.5)+
  geom_line(aes(y=WM), colour="#1f78b4", size=0.5)+
  geom_line(aes(y=IW), colour="#00008b", size=0.5)+
  geom_line(aes(y=NNO), colour="orange", size=0.5)+
  geom_line(aes(y=MH), colour="red", size=0.5)+
  geom_line(aes(y=QIF), colour="black", size=0.5)+
  geom_line(aes(y=CHO), colour="grey50", linetype=2, size=0.5)+
  geom_line(aes(y=H2O), colour="blue", linetype=2, size=0.5)+
  geom_line(aes(y=Nb4_5), colour="#ff69b4", linetype=2)+
  geom_line(aes(y=Ta0_5), colour="#c73794", linetype=2)+  
  geom_line(aes(y=SnSnO2), colour="purple", linetype=2, size=0.5)+
  geom_line(aes(y=W4_6), colour="#204080", linetype=2)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent")
        )+
  scale_x_continuous("T (K)", sec.axis = sec_axis(~.-273.15, name=expression("T " ( degree*C))))+
  scale_y_continuous(expression("log"[10]~italic("f")[O[2]]), sec.axis = dup_axis(name=NULL))+
  coord_cartesian(xlim=c(573,1073), ylim=c(-50,0))
  
print(buffers)
w<-6
ggsave("buffers.pdf", buffers, width=w, height=w/sqrt(2))