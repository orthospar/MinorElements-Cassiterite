#organise summary statistics tables for printing

library(dplyr)
source("C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/EPMA Transects/import.R")
data<-bdl(data,0)
meanNaN <- function(x){mean(x[!is.nan(x)])}

#Atomic %
#calculate summary stats
table <- data %>%
  group_by(LOCALITY) %>%
  summarise_each(funs(mean, sd, max, median, min, length), 
         Sn.AT., Ti.AT., Fe.AT., Mn.AT.,
         Nb.AT., Ta.AT., W.AT., Zr.AT., O.AT.) 

#convert cdl99 (in wt%) to at%
#no detlim for O
detlim <-data %>% 
  mutate(Sn=(Sn.AT./Sn.WT.)*Sn.CDL99,
         Ti=(Ti.AT./Ti.WT.)*Ti.CDL99,
         Fe=(Fe.AT./Fe.WT.)*Fe.CDL99,
         Mn=(Mn.AT./Mn.WT.)*Mn.CDL99,
         Nb=(Nb.AT./Nb.WT.)*Nb.CDL99,
         Ta=(Ta.AT./Ta.WT.)*Ta.CDL99,
         W=(W.AT./W.WT.)*W.CDL99,
         Zr=(Zr.AT./Zr.WT.)*Zr.CDL99) %>%
  summarise_each(funs(meanNaN), Sn, Ti, Fe, Mn, Nb, Ta, W, Zr) %>%
  cbind(O=NA,Locality=NA,n=NA,"Atomic %"="Detection Limit")

#calculate mean uncertainties only on cases >cdl99
uncert <-data %>%
  mutate(Sn=mean(Sn.AT.[Sn.WT.>Sn.CDL99]*(Sn..ERR/100)[Sn.WT.>Sn.CDL99]),
         Ti=mean(Ti.AT.[Ti.WT.>Ti.CDL99]*(Ti..ERR/100)[Ti.WT.>Ti.CDL99]),
         Fe=mean(Fe.AT.[Fe.WT.>Fe.CDL99]*(Fe..ERR/100)[Fe.WT.>Fe.CDL99]),
         Mn=mean(Mn.AT.[Mn.WT.>Mn.CDL99]*(Mn..ERR/100)[Mn.WT.>Mn.CDL99]),
         Nb=mean(Nb.AT.[Nb.WT.>Nb.CDL99]*(Nb..ERR/100)[Nb.WT.>Nb.CDL99]),
         Ta=mean(Ta.AT.[Ta.WT.>Ta.CDL99]*(Ta..ERR/100)[Ta.WT.>Ta.CDL99]),
         W=mean(W.AT.[W.WT.>W.CDL99]*(W..ERR/100)[W.WT.>W.CDL99]),
         Zr=mean(Zr.AT.[Zr.WT.>Zr.CDL99]*(Zr..ERR/100)[Zr.WT.>Zr.CDL99]))%>%
  summarise_each(funs(mean), Sn, Ti, Fe, Mn, Nb, Ta, W, Zr) %>%
  cbind(O=NA,Locality=NA,n=NA,"Atomic %"="Uncertainty")

#reshape table to print-friendly format
names <- c("Locality","n","Sn","Ti","Fe","Mn","Nb","Ta","W","Zr","O","Atomic %")
f1<-table[,c(1,55,2:10)] %>% cbind(Atomic=(rep("mean",18)))
f2<-table[,c(1,55,11:19)]%>% cbind(Atomic=(rep("sd",18)))
f3<-table[,c(1,55,20:28)]%>% cbind(Atomic=(rep("max",18)))
f4<-table[,c(1,55,29:37)]%>% cbind(Atomic=(rep("median",18)))
f5<-table[,c(1,55,38:46)]%>% cbind(Atomic=(rep("min",18)))
colnames(f1) <- names
colnames(f2) <- names
colnames(f3) <- names
colnames(f4) <- names
colnames(f5) <- names

#combine and organise the parts
atomic<-rbind.data.frame(f1,f2,f3,f4,f5)
atomic<-arrange(atomic,Locality)
atomic<-atomic %>%  #insert blank rows between localities
  group_by(Locality) %>% 
  do(rbind(.,c(rep(NA,NCOL(atomic))))) %>%
  ungroup() %>% data.frame()
colnames(atomic) <- names #restore % sign in name
atomic<-rbind.data.frame(detlim[,c(10:12,1:9)],
                         uncert[,c(10:12,1:9)],
                         c(rep(NA,NCOL(atomic))),
                         atomic[,c(1:2,12,3:11)])
keep<-function(x){6*x+4}
atomic[-keep(0:17),1:2]<-NA

#Weight %
#calculate summary stats
table <- data %>%
  group_by(LOCALITY) %>%
  summarise_each(funs(mean, sd, max, median, min, length),
                 Sn.WT., Ti.WT., Fe.WT., Mn.WT.,
                 Nb.WT., Ta.WT., W.WT., Zr.WT., O.WT.)

#only need to calculate means here
#no detlim for O
detlim <-data %>%
  mutate(Sn=Sn.CDL99, Ti=Ti.CDL99, Fe=Fe.CDL99, Mn=Mn.CDL99,
         Nb=Nb.CDL99, Ta=Ta.CDL99, W=W.CDL99, Zr=Zr.CDL99) %>%
  summarise_each(funs(meanNaN), Sn, Ti, Fe, Mn, Nb, Ta, W, Zr) %>%
  cbind(O=NA,Locality=NA,n=NA,"Weight %"="Detection Limit")

#calculate uncertainties only on cases >cdl99
uncert <-data %>%
  mutate(Sn=mean(Sn.WT.[Sn.WT.>Sn.CDL99]*(Sn..ERR/100)[Sn.WT.>Sn.CDL99]),
         Ti=mean(Ti.WT.[Ti.WT.>Ti.CDL99]*(Ti..ERR/100)[Ti.WT.>Ti.CDL99]),
         Fe=mean(Fe.WT.[Fe.WT.>Fe.CDL99]*(Fe..ERR/100)[Fe.WT.>Fe.CDL99]),
         Mn=mean(Mn.WT.[Mn.WT.>Mn.CDL99]*(Mn..ERR/100)[Mn.WT.>Mn.CDL99]),
         Nb=mean(Nb.WT.[Nb.WT.>Nb.CDL99]*(Nb..ERR/100)[Nb.WT.>Nb.CDL99]),
         Ta=mean(Ta.WT.[Ta.WT.>Ta.CDL99]*(Ta..ERR/100)[Ta.WT.>Ta.CDL99]),
         W=mean(W.WT.[W.WT.>W.CDL99]*(W..ERR/100)[W.WT.>W.CDL99]),
         Zr=mean(Zr.WT.[Zr.WT.>Zr.CDL99]*(Zr..ERR/100)[Zr.WT.>Zr.CDL99]))%>%
  summarise_each(funs(mean), Sn, Ti, Fe, Mn, Nb, Ta, W, Zr) %>%
  cbind(O=NA,Locality=NA,n=NA,"Weight %"="Uncertainty")

#reshape datatable to print-friendly format
names<- c("Locality","n","Sn","Ti","Fe","Mn","Nb","Ta","W","Zr","O","Weight %")
f1<-table[,c(1,55,2:10)] %>% cbind(f=(rep("mean",18)))
f2<-table[,c(1,55,11:19)]%>% cbind(f=(rep("sd",18)))
f3<-table[,c(1,55,20:28)]%>% cbind(f=(rep("max",18)))
f4<-table[,c(1,55,29:37)]%>% cbind(f=(rep("median",18)))
f5<-table[,c(1,55,38:46)]%>% cbind(f=(rep("min",18)))
colnames(f1) <- names
colnames(f2) <- names
colnames(f3) <- names
colnames(f4) <- names
colnames(f5) <- names

#combine and organise the parts
weight<-rbind.data.frame(f1,f2,f3,f4,f5)
weight<-arrange(weight,Locality)
weight<-weight %>% 
  group_by(Locality) %>% 
  do(rbind(.,c(rep(NA,NCOL(weight))))) %>%
  ungroup() %>% data.frame()
colnames(weight)<- c("Locality","n","Sn","Ti","Fe","Mn","Nb","Ta","W","Zr","O","Weight %")
weight<-rbind.data.frame(detlim[,c(10:12,1:9)],
                         uncert[,c(10:12,1:9)],
                         c(rep(NA,NCOL(weight))),
                         weight[,c(1:2,12,3:11)])
weight[-keep(0:17),1:2]<-NA

#Weight % Oxide
#calculate summary stats
table <- data %>%
  group_by(LOCALITY) %>%
  summarise_each(funs(mean, sd, max, median, min, length),
                 SnO2, TiO2, FeO, MnO,
                 Nb2O5, Ta2O5, WO3, ZrO2)

#Also only need to calculate means
detlim <-data %>%
  mutate(SnO2=SnO2.CDL99,TiO2=TiO2.CDL99, FeO=FeO.CDL99, MnO=MnO.CDL99,
         Nb2O5=Nb2O5.CDL99, Ta2O5=Ta2O5.CDL99, WO3=WO3.CDL99, ZrO2=ZrO2.CDL99) %>%
  summarise_each(funs(meanNaN), SnO2, TiO2, FeO, MnO, Nb2O5, Ta2O5, WO3, ZrO2) %>%
  cbind(Locality=NA,n=NA,"Wt% Oxide"="Detection Limit")

#calculate uncertainties only on cases >cdl99
uncert <-data %>%
  mutate(SnO2=mean(SnO2[Sn.WT.>Sn.CDL99]*(Sn..ERR/100)[Sn.WT.>Sn.CDL99]),
          TiO2=mean(TiO2[Ti.WT.>Ti.CDL99]*(Ti..ERR/100)[Ti.WT.>Ti.CDL99]),
          FeO=mean(FeO[Fe.WT.>Fe.CDL99]*(Fe..ERR/100)[Fe.WT.>Fe.CDL99]),
          MnO=mean(MnO[Mn.WT.>Mn.CDL99]*(Mn..ERR/100)[Mn.WT.>Mn.CDL99]),
          Nb2O5=mean(Nb2O5[Nb.WT.>Nb.CDL99]*(Nb..ERR/100)[Nb.WT.>Nb.CDL99]),
          Ta2O5=mean(Ta2O5[Ta.WT.>Ta.CDL99]*(Ta..ERR/100)[Ta.WT.>Ta.CDL99]),
          WO3=mean(WO3[W.WT.>W.CDL99]*(W..ERR/100)[W.WT.>W.CDL99]),
          ZrO2=mean(ZrO2[Zr.WT.>Zr.CDL99]*(Zr..ERR/100)[Zr.WT.>Zr.CDL99]))%>%
  summarise_each(funs(mean), SnO2, TiO2, FeO, MnO, Nb2O5, Ta2O5, WO3, ZrO2) %>%
  cbind(Locality=NA,n=NA,"Wt% Oxide"="Uncertainty")

#reshape datatable to print-friendly format
names <- c("Locality","n", "SnO2", "TiO2", "FeO", "MnO", "Nb2O5", "Ta2O5", "WO3", "ZrO2","Wt% Oxide")
f1<-table[,c(1,42,2:9)] %>% cbind(f=(rep("mean",18)))
colnames(f1) <- names
f2<-table[,c(1,42,10:17)]%>% cbind(f=(rep("sd",18)))
colnames(f2) <- names
f3<-table[,c(1,42,18:25)]%>% cbind(f=(rep("max",18)))
colnames(f3) <- names
f4<-table[,c(1,42,26:33)]%>% cbind(f=(rep("median",18)))
colnames(f4) <- names
f5<-table[,c(1,42,34:41)]%>% cbind(f=(rep("min",18)))
colnames(f5) <- names

#combine and organise the parts
oxide<-rbind.data.frame(f1,f2,f3,f4,f5)
oxide<-arrange(oxide,Locality)
oxide<-oxide %>% 
  group_by(Locality) %>% 
  do(rbind(.,c(rep(NA,NCOL(oxide))))) %>%
  ungroup() %>% data.frame()
colnames(oxide)<-names
oxide<-rbind.data.frame(detlim[,c(9:11,1:8)],
                        uncert[,c(9:11,1:8)],
                        c(rep(NA,NCOL(oxide))),
                        oxide[,c(1:2,11,3:10)])
oxide[-keep(0:17),1:2]<-NA

#Mol % Oxide
#calculate summary stats
table <- data %>%
  group_by(LOCALITY) %>%
  summarise_each(funs(mean, sd, max, median, min, length),
                 Sn.OXMOL., Ti.OXMOL., Fe.OXMOL., Mn.OXMOL.,
                 Nb.OXMOL., Ta.OXMOL., W.OXMOL., Zr.OXMOL.)

#convert cdl99 (in wt% oxide) to mol% oxide
detlim <-data %>%
  mutate(SnO2=(Sn.OXMOL./SnO2)*SnO2.CDL99,
         TiO2=(Ti.OXMOL./TiO2)*TiO2.CDL99,
         FeO=(Fe.OXMOL./FeO)*FeO.CDL99,
         MnO=(Mn.OXMOL./MnO)*MnO.CDL99,
         Nb2O5=(Nb.OXMOL./Nb2O5)*Nb2O5.CDL99,
         Ta2O5=(Ta.OXMOL./Ta2O5)*Ta2O5.CDL99,
         WO3=(W.OXMOL./WO3)*WO3.CDL99,
         ZrO2=(Zr.OXMOL./ZrO2)*ZrO2.CDL99) %>%
  summarise_each(funs(meanNaN), SnO2, TiO2, FeO, MnO, Nb2O5, Ta2O5, WO3, ZrO2) %>%
  cbind(Locality=NA,n=NA,"Mol% Oxide"="Detection Limit")

#calculate uncertainties only on cases >cdl99
uncert <-data %>%
  mutate(SnO2=mean(Sn.OXMOL.[Sn.WT.>Sn.CDL99]*(Sn..ERR/100)[Sn.WT.>Sn.CDL99]),
         TiO2=mean(Ti.OXMOL.[Ti.WT.>Ti.CDL99]*(Ti..ERR/100)[Ti.WT.>Ti.CDL99]),
         FeO=mean(Fe.OXMOL.[Fe.WT.>Fe.CDL99]*(Fe..ERR/100)[Fe.WT.>Fe.CDL99]),
         MnO=mean(Mn.OXMOL.[Mn.WT.>Mn.CDL99]*(Mn..ERR/100)[Mn.WT.>Mn.CDL99]),
         Nb2O5=mean(Nb.OXMOL.[Nb.WT.>Nb.CDL99]*(Nb..ERR/100)[Nb.WT.>Nb.CDL99]),
         Ta2O5=mean(Ta.OXMOL.[Ta.WT.>Ta.CDL99]*(Ta..ERR/100)[Ta.WT.>Ta.CDL99]),
         WO3=mean(W.OXMOL.[W.WT.>W.CDL99]*(W..ERR/100)[W.WT.>W.CDL99]),
         ZrO2=mean(Zr.OXMOL.[Zr.WT.>Zr.CDL99]*(Zr..ERR/100)[Zr.WT.>Zr.CDL99]))%>%
  summarise_each(funs(mean), SnO2, TiO2, FeO, MnO, Nb2O5, Ta2O5, WO3, ZrO2) %>%
  cbind(Locality=NA,n=NA,"Mol% Oxide"="Uncertainty")

#reshape datatable to print-friendly format
names <- c("Locality","n", "SnO2", "TiO2", "FeO", "MnO", "Nb2O5", "Ta2O5", "WO3", "ZrO2","Mol% Oxide")
f1<-table[,c(1,42,2:9)] %>% cbind(f=(rep("mean",18)))
colnames(f1) <- names
f2<-table[,c(1,42,10:17)]%>% cbind(f=(rep("sd",18)))
colnames(f2) <- names
f3<-table[,c(1,42,18:25)]%>% cbind(f=(rep("max",18)))
colnames(f3) <- names
f4<-table[,c(1,42,26:33)]%>% cbind(f=(rep("median",18)))
colnames(f4) <- names
f5<-table[,c(1,42,34:41)]%>% cbind(f=(rep("min",18)))
colnames(f5) <- names

#combine and organise the parts
oxmol<-rbind.data.frame(f1,f2,f3,f4,f5)
oxmol<-arrange(oxmol,Locality)
oxmol<-oxmol %>% 
  group_by(Locality) %>% 
  do(rbind(.,c(rep(NA,NCOL(oxmol))))) %>%
  ungroup() %>% data.frame()
colnames(oxmol)<-names
oxmol<-rbind.data.frame(detlim[,c(9:11,1:8)],
                        uncert[,c(9:11,1:8)],
                        c(rep(NA,NCOL(oxmol))),
                        oxmol[,c(1:2,11,3:10)])
oxmol[-keep(0:17),1:2]<-NA

#% Atoms per formula unit
#calculate summary stats
table <- data %>%
  group_by(LOCALITY) %>%
  summarise_each(funs(mean, sd, max, median, min, length), 
                 Sn.FORMULA, Ti.FORMULA, Fe.FORMULA, Mn.FORMULA,
                 Nb.FORMULA, Ta.FORMULA, W.FORMULA, Zr.FORMULA)
table[,2:41] <- table[,2:41]*100


#convert cdl99 (in wt%) to %APFU
#no detlim for O
detlim <-data %>% 
  mutate(Sn=(Sn.FORMULA/Sn.WT.)*Sn.CDL99*100,
         Ti=(Ti.FORMULA/Ti.WT.)*Ti.CDL99*100,
         Fe=(Fe.FORMULA/Fe.WT.)*Fe.CDL99*100,
         Mn=(Mn.FORMULA/Mn.WT.)*Mn.CDL99*100,
         Nb=(Nb.FORMULA/Nb.WT.)*Nb.CDL99*100,
         Ta=(Ta.FORMULA/Ta.WT.)*Ta.CDL99*100,
         W=(W.FORMULA/W.WT.)*W.CDL99*100,
         Zr=(Zr.FORMULA/Zr.WT.)*Zr.CDL99*100) %>%
  summarise_each(funs(meanNaN), Sn, Ti, Fe, Mn, Nb, Ta, W, Zr) %>%
  cbind(Locality=NA,n=NA,APFU="Detection Limit")

#calculate uncertainties only on cases >cdl99
uncert <-data %>%
  mutate(Sn=mean(Sn.FORMULA[Sn.WT.>Sn.CDL99]*(Sn..ERR)[Sn.WT.>Sn.CDL99]),
         Ti=mean(Ti.FORMULA[Ti.WT.>Ti.CDL99]*(Ti..ERR)[Ti.WT.>Ti.CDL99]),
         Fe=mean(Fe.FORMULA[Fe.WT.>Fe.CDL99]*(Fe..ERR)[Fe.WT.>Fe.CDL99]),
         Mn=mean(Mn.FORMULA[Mn.WT.>Mn.CDL99]*(Mn..ERR)[Mn.WT.>Mn.CDL99]),
         Nb=mean(Nb.FORMULA[Nb.WT.>Nb.CDL99]*(Nb..ERR)[Nb.WT.>Nb.CDL99]),
         Ta=mean(Ta.FORMULA[Ta.WT.>Ta.CDL99]*(Ta..ERR)[Ta.WT.>Ta.CDL99]),
         W=mean(W.FORMULA[W.WT.>W.CDL99]*(W..ERR)[W.WT.>W.CDL99]),
         Zr=mean(Zr.FORMULA[Zr.WT.>Zr.CDL99]*(Zr..ERR)[Zr.WT.>Zr.CDL99]))%>%
  summarise_each(funs(mean), Sn, Ti, Fe, Mn, Nb, Ta, W, Zr) %>%
  cbind(Locality=NA,n=NA,APFU="Uncertainty")

#reshape table to print-friendly format
names <- c("Locality","n","Sn","Ti","Fe","Mn","Nb","Ta","W","Zr","APFU")
f1<-table[,c(1,42,2:9)] %>% cbind(f=(rep("mean",18)))
colnames(f1) <- names
f2<-table[,c(1,42,10:17)]%>% cbind(f=(rep("sd",18)))
colnames(f2) <- names
f3<-table[,c(1,42,18:25)]%>% cbind(f=(rep("max",18)))
colnames(f3) <- names
f4<-table[,c(1,42,26:33)]%>% cbind(f=(rep("median",18)))
colnames(f4) <- names
f5<-table[,c(1,42,34:41)]%>% cbind(f=(rep("min",18)))
colnames(f5) <- names

#combine and organise the parts
apfu<-rbind.data.frame(f1,f2,f3,f4,f5)
apfu<-arrange(apfu,Locality)
apfu<-apfu %>%  #insert blank rows between localities
  group_by(Locality) %>% 
  do(rbind(.,c(rep(NA,NCOL(apfu))))) %>%
  ungroup() %>% data.frame()
apfu<-rbind.data.frame(detlim[,c(9:11,1:8)],
                       uncert[,c(9:11,1:8)],
                       c(rep(NA,NCOL(apfu))),
                       apfu[,c(1:2,11,3:10)])
apfu[-keep(0:17),1:2]<-NA

#save function so I only overwrite the files when I want to
save <- function(){
  savelocation<-"C:/Users/Jason/OneDrive - research.uwa.edu.au/Publications/Papers/Paper2/tables/"
  write.csv(atomic, paste0(savelocation, "Summary_Atomic.csv"), na="", row.names = FALSE)
  write.csv(weight, paste0(savelocation, "Summary_Weight.csv"), na="", row.names = FALSE)
  write.csv(oxide, paste0(savelocation, "Summary_Oxide.csv"), na="", row.names = FALSE)
  write.csv(oxmol, paste0(savelocation, "Summary_Oxmol.csv"), na="", row.names = FALSE)
  write.csv(apfu, paste0(savelocation, "Summary_APFU.csv"), na="", row.names = FALSE)
  rm(savelocation)
  }
#save()
#clean up
rm(keep, detlim, f1, f2, f3, f4, f5, table, uncert, names)