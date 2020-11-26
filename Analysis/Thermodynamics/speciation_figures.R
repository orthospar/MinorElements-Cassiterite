setwd("C:/Users/Jason/OneDrive - research.uwa.edu.au/Analysis/Thermodynamics")
library(openxlsx)
library(ggplot2)

S<-read.xlsx("thermodynamics.xlsx", sheet="S") #Standard Entropy table, in J mol-1 K-1
H<-read.xlsx("thermodynamics.xlsx", sheet="dH")  #Standard Enthalpy of Formation table, in kJ mol-1 K-1
H[,-1]<-H[,-1]*1000 #convert enthalpy from kJ to J
R<-8.3144589 #Gas constant, J mol-1 K-1

#select the temperature of interest (limited to T intervals from table)
T_i <- which(S$TK==800)

#calculate the mid-point for NNO at the temperature of interest
fO2_NNO <- ((((2*H$NiO)-(2*H$Ni+H$O2))-S$TK*((2*S$NiO)-(2*S$Ni+S$O2)))/(R*S$TK*log(10)))[T_i]

#Calculate the mid-points for each buffer reaction as a difference from NNO
dNNO <- data.frame(
#the main buffer reactions  
QIF   = ((((H$fayalite)-(H$quartz+2*H$Fe+H$O2))-S$TK*((S$fayalite)-(S$quartz+2*S$Fe+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO,
QFM   = ((((2*H$magnetite+3*H$quartz)-(3*H$fayalite+H$O))-S$TK*((2*S$magnetite+3*S$quartz)-(3*S$fayalite+S$O)))/(R*S$TK*log(10)))[T_i]-fO2_NNO,
MH    = ((((6*H$hematite)-(4*H$magnetite+H$O2))-S$TK*((6*S$hematite)-(4*S$magnetite+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO,
WM    = ((((2*H$magnetite)-(6*H$FeO+H$O2))-S$TK*((2*S$magnetite)-(6*S$FeO+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO,
CHO   = ((((0.5*H$CO2+H$H2O)-(0.5*H$CH4+H$O2))-S$TK*((0.5*S$CO2+S$H2O)-(0.5*S$CH4+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO,
IW    = ((((2*H$FeO)-(2*H$Fe+H$O2))-S$TK*((2*S$FeO)-(2*S$Fe+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO,
WH    = ((((2*H$hematite)-(4*H$FeO+H$O2))-S$TK*((2*S$hematite)-(4*S$FeO+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO,
H2O   = ((((2*H$H2O)-(2*H$H2+H$O2))-S$TK*((2*S$H2O)-(2*S$H2+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO,
#the buffer reactions of interest
Nb4_5 = ((((2*H$Nb2O5)-(4*H$NbO2+H$O2))-S$TK*((2*S$Nb2O5)-(4*S$NbO2+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO,
Ta0_5 = (((((2/5)*H$Ta2O5)-((4/5)*H$Ta+H$O2))-S$TK*(((2/5)*S$Ta2O5)-((4/5)*S$Ta+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO,
SnSnO2= ((((H$SnO2)-(H$Sn+H$O2))-S$TK*((S$SnO2)-(S$Sn+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO,
W4_6  = ((((2*H$WO3)-(2*H$WO2+H$O2))-S$TK*((2*S$WO3)-(2*S$WO2+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO,
Ti3_4 = ((((4*H$TiO2)-(2*H$Ti2O3+H$O2))-S$TK*((4*S$TiO2)-(2*S$Ti2O3+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO,
Mn3_4 = ((((4*H$MnO2)-(2*H$Mn2O3+H$O2))-S$TK*((4*S$MnO2)-(2*S$Mn2O3+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO,
Mn2_3 = ((((2*H$Mn2O3)-(4*H$MnO+H$O2))-S$TK*((2*S$Mn2O3)-(4*S$MnO+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO,
Mn0_2 = ((((2*H$MnO)-(2*H$Mn+H$O2))-S$TK*((2*S$MnO)-(2*S$Mn+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO
)

#calculate the k-terms
k <- data.frame(
Nb = 10^((dNNO$Nb4_5*1)/-4),
Ta = 10^((dNNO$Ta0_5*5)/-4),
W  = 10^((dNNO$W4_6*2)/-4),
Sn = 10^((dNNO$SnSnO2*4)/-4),
Ti = 10^((dNNO$Ti3_4*1)/-4),
Mn4= 10^((dNNO$Mn3_4*1)/-4),
Mn3= 10^((dNNO$Mn2_3*1)/-4),
Mn2= 10^((dNNO$Mn0_2*2)/-4),
Fe2= 10^((dNNO$IW*2/-4)),
Fe3= 10^((dNNO$WH*1/-4))
)

#The range in fO2 (units of dNNO) to model/plot
fO2_range <- seq(-50,30,0.1)

#calculate the speciation ratio of highest state across that range
speciation <- data.frame(
fO2_range,
Nb5  = (k$Nb*10^((fO2_range*1)/4))/(1+(k$Nb*10^((fO2_range*1)/4))),   #Nb4 to Nb5
Ta5  = (k$Ta*10^((fO2_range*5)/4))/(1+(k$Ta*10^((fO2_range*5)/4))),   #Ta0 to Ta5
W6   = (k$W*10^((fO2_range*2)/4))/(1+(k$W*10^((fO2_range*2)/4))),     #W4  to W6
Sn4  = (k$Sn*10^((fO2_range*4)/4))/(1+(k$Sn*10^((fO2_range*4)/4))),   #Sn0 to Sn4
Ti4  = (k$Ti*10^((fO2_range*1)/4))/(1+(k$Ti*10^((fO2_range*1)/4))),   #Ti3 to Ti4
Mn4  = (k$Mn4*10^((fO2_range*1)/4))/(1+(k$Mn4*10^((fO2_range*1)/4))), #Mn3 to Mn4
Mn3  = (k$Mn3*10^((fO2_range*1)/4))/(1+(k$Mn3*10^((fO2_range*1)/4))), #Mn2 to Mn3
Mn2  = (k$Mn2*10^((fO2_range*2)/4))/(1+(k$Mn2*10^((fO2_range*2)/4))), #Mn0 to Mn2
Fe3  = (k$Fe3*10^((fO2_range*1)/4))/(1+(k$Fe3*10^((fO2_range*1)/4))), #Fe2 to Fe3
Fe2  = (k$Fe2*10^((fO2_range*2)/4))/(1+(k$Fe2*10^((fO2_range*2)/4))) #Fe0 to Fe2
)


#Build the base plot
p <- ggplot(speciation, aes(x=fO2_range))+
  annotate(geom="rect", xmin=dNNO$H2O, xmax=0, ymin=0, ymax=1, fill="grey80")+
  geom_vline(xintercept = dNNO$MH, colour="red", linetype=2)+
  geom_vline(xintercept = dNNO$QFM, colour="#33a02c", linetype=2)+
  geom_vline(xintercept = dNNO$WM, colour="#1f78b4", linetype=2)+
  geom_vline(xintercept = dNNO$QIF, colour="black", linetype=2)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.title.y = element_text(angle = 0, vjust = 0.5))+
  coord_cartesian(xlim=c(-25,25), ylim=c(0,1.01), expand=FALSE)+
  labs(x=expression("log"[10]~italic("f")[O[2]]~(Delta*"NNO")), y=expression(over(" M"^"n+",Sigma*"M")))

p2 <- p +
  geom_line(aes(y=Ti4), colour="black")+
  geom_line(aes(y=Nb5), colour="hot pink")+
  geom_line(aes(y=Ta5), colour="#c73794")+
  geom_line(aes(y=W6), colour="#204080")

p3 <- p +
  geom_line(aes(y=(Mn2-(Mn3+Mn4))), colour="black")+ #total Mn2
  geom_line(aes(y=(Mn3-(Mn4))), colour="grey40")+    #total Mn3
  geom_line(aes(y=Mn4), colour="grey60")

p1 <- p+
  geom_line(aes(y=1-(Fe2+Fe3)), colour="black")+     #total Fe0
  geom_line(aes(y=Fe2-Fe3), colour="grey40")+        #total Fe2
  geom_line(aes(y=Fe3), colour="grey60")+
  geom_line(aes(y=Sn4), colour="purple")

#at CH4:CO2, mole fraction of Nb4+ is:
#1-(k$Nb*10^((dNNO$CHO*1)/4))/(1+(k$Nb*10^((dNNO$CHO*1)/4)))
#0.02156166, ~2.16%
#at Sn:SnO2, mole fraction of Nb4+ is:
#1-(k$Nb*10^((dNNO$SnSnO2*1)/4))/(1+(k$Nb*10^((dNNO$SnSnO2*1)/4)))
#0.03011573, ~3.01%
#0.01774869 @ NNO-4, ~1.77%
#0.03113209 @ NNO-5, ~3.11% *****Most likely lower limit*****
#0.0540519 @ NNO-6, ~5.41%
#0.09223912 @ NNO-7, ~9.22%

w<-6
ggsave("Mn.pdf", p3, width=w, height=w/sqrt(2)) 
ggsave("Fe.pdf", p1, width=w, height=w/sqrt(2))
ggsave("Ti.pdf", p2, width=w, height=w/sqrt(2))
