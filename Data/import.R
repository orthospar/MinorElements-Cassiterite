#move to data directory, rather than using long file paths
wd <- getwd()
setwd('C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/EPMA Transects')


#A function to assign NA, 0 or cdl99/2 to values below the 99% confidence level (cdl99)
#The confidence level is reported in wt%, so the function compares the value in wt% with the cdl99
#Then sets the value to cdl99/2 calculated for the correct unit if asked

#NA functionality not yet working
#half equations need updating (only inserts half the wt% cdl)
bdl <- function(EPMA_data, x){#where EPMA_data=data.frame, x=NA|0|half
  EPMA_data <- within(EPMA_data, {
    if(x!="half"){
      Fe.WT.[Fe.WT. < Fe.CDL99] <- x
      Ti.WT.[Ti.WT. < Ti.CDL99] <- x
      Nb.WT.[Nb.WT. < Nb.CDL99] <- x
      Ta.WT.[Ta.WT. < Ta.CDL99] <- x
      Mn.WT.[Mn.WT. < Mn.CDL99] <- x
      Zr.WT.[Zr.WT. < Zr.CDL99] <- x
      W.WT.[W.WT. < W.CDL99] <- x
      Fe.AT.[Fe.WT. < Fe.CDL99] <- x
      Ti.AT.[Ti.WT. < Ti.CDL99] <- x
      Nb.AT.[Nb.WT. < Nb.CDL99] <- x
      Ta.AT.[Ta.WT. < Ta.CDL99] <- x
      Mn.AT.[Mn.WT. < Mn.CDL99] <- x
      Zr.AT.[Zr.WT. < Zr.CDL99] <- x
      W.AT.[W.WT. < W.CDL99] <- x
      Fe.FORMULA[Fe.WT. < Fe.CDL99] <- x
      Ti.FORMULA[Ti.WT. < Ti.CDL99] <- x
      Nb.FORMULA[Nb.WT. < Nb.CDL99] <- x
      Ta.FORMULA[Ta.WT. < Ta.CDL99] <- x
      Mn.FORMULA[Mn.WT. < Mn.CDL99] <- x
      Zr.FORMULA[Zr.WT. < Zr.CDL99] <- x
      W.FORMULA[W.WT. < W.CDL99] <- x
      FeO[FeO < FeO.CDL99] <- x
      TiO2[TiO2 < TiO2.CDL99] <- x
      Nb2O5[Nb2O5 < Nb2O5.CDL99] <- x
      Ta2O5[Ta2O5 < Ta2O5.CDL99] <- x
      MnO[MnO < MnO.CDL99] <- x
      ZrO2[ZrO2 < ZrO2.CDL99] <- x
      WO3[WO3 < WO3.CDL99] <- x
      Fe.OXMOL.[Fe.WT. < Fe.CDL99] <- x
      Ti.OXMOL.[Ti.WT. < Ti.CDL99] <- x
      Nb.OXMOL.[Nb.WT. < Nb.CDL99] <- x
      Ta.OXMOL.[Ta.WT. < Ta.CDL99] <- x
      Mn.OXMOL.[Mn.WT. < Mn.CDL99] <- x
      Zr.OXMOL.[Zr.WT. < Zr.CDL99] <- x
      W.OXMOL.[W.WT. < W.CDL99] <- x
    }
    else{
      Fe.WT.[Fe.WT. < Fe.CDL99] <- mean(Fe.CDL99)/2
      Ti.WT.[Ti.WT. < Ti.CDL99] <- mean(Ti.CDL99)/2
      Nb.WT.[Nb.WT. < Nb.CDL99] <- mean(Nb.CDL99)/2
      Ta.WT.[Ta.WT. < Ta.CDL99] <- mean(Ta.CDL99)/2
      Mn.WT.[Mn.WT. < Mn.CDL99] <- mean(Mn.CDL99)/2
      Zr.WT.[Zr.WT. < Zr.CDL99] <- mean(Zr.CDL99)/2
      W.WT.[W.WT. < W.CDL99] <- mean(W.CDL99)/2
      Fe.AT.[Fe.WT. < Fe.CDL99] <- mean(Fe.CDL99)/2
      Ti.AT.[Ti.WT. < Ti.CDL99] <- mean(Ti.CDL99)/2
      Nb.AT.[Nb.WT. < Nb.CDL99] <- mean(Nb.CDL99)/2
      Ta.AT.[Ta.WT. < Ta.CDL99] <- mean(Ta.CDL99)/2
      Mn.AT.[Mn.WT. < Mn.CDL99] <- mean(Mn.CDL99)/2
      Zr.AT.[Zr.WT. < Zr.CDL99] <- mean(Zr.CDL99)/2
      W.AT.[W.WT. < W.CDL99] <- mean(W.CDL99)/2
      Fe.FORMULA[Fe.WT. < Fe.CDL99] <- mean(FeO.CDL99)/2
      Ti.FORMULA[Ti.WT. < Ti.CDL99] <- mean(TiO2.CDL99)/2
      Nb.FORMULA[Nb.WT. < Nb.CDL99] <- mean(Nb2O5.CDL99)/2
      Ta.FORMULA[Ta.WT. < Ta.CDL99] <- mean(Ta2O5.CDL99)/2
      Mn.FORMULA[Mn.WT. < Mn.CDL99] <- mean(MnO.CDL99)/2
      Zr.FORMULA[Zr.WT. < Zr.CDL99] <- mean(ZrO2.CDL99)/2
      W.FORMULA[W.WT. < W.CDL99] <- mean(WO3.CDL99)/2
      FeO[FeO < FeO.CDL99] <- mean(FeO.CDL99)/2
      TiO2[TiO2 < TiO2.CDL99] <- mean(TiO2.CDL99)/2
      Nb2O5[Nb2O5 < Nb2O5.CDL99] <- mean(Nb2O5.CDL99)/2
      Ta2O5[Ta2O5 < Ta2O5.CDL99] <- mean(Ta2O5.CDL99)/2
      MnO[MnO < MnO.CDL99] <- mean(MnO.CDL99)/2
      ZrO2[ZrO2 < ZrO2.CDL99] <- mean(ZrO2.CDL99)/2
      WO3[WO3 < WO3.CDL99] <- mean(WO3.CDL99)/2
      }  
  })
}

#for each EPMA .csv file:
#---read the .csv
#---define which analyses are to be excluded - this is a manually curated list of the values in the LINE column that correspond to inclusions or spurious totals
#---remove these analyses
#---set the values below cdl99 to NA

#run1: Sn totals too high (>100%) due to standard issues. Data excluded.

#run2: Standard issue resolved. Transects on crystals from Mt Bischoff and Sifleets Reward
run2 <- read.csv("161021/161021.csv",stringsAsFactors=FALSE)
obs<-read.csv("161021/161021obs.csv",stringsAsFactors=FALSE)
run2 <- dplyr::bind_cols(obs[,-1], run2)
filter_lines <- c(149:152, 164:166, 210, 277, 282:283, 305, 359, 278, 360, 376:378)
run2 <- run2[!(run2$LINE %in% filter_lines),]
run2 <- dplyr::tbl_df(run2)

#run3: Unstable beam. Data excluded.

#run4: Elsmore Transects
run4 <- read.csv("170609/170609.csv",stringsAsFactors=FALSE)
obs<-read.csv("170609/170609obs.csv",stringsAsFactors=FALSE)
run4 <- dplyr::bind_cols(obs[,-1], run4)
filter_lines <- c(153,164,176,265,332:335,405,513,605,735,759,780,797,841,843:844,997:998,1140,1189:1190,1215,1246)
run4 <- run4[!(run4$LINE %in% filter_lines),]
run4 <- dplyr::tbl_df(run4)

#run5: Aberfoyle Transects, in two parts after crash and reset.
run5a <- read.csv("170811/170811.csv",stringsAsFactors=FALSE)
obs<-read.csv("170811/170811obs.csv",stringsAsFactors=FALSE)
run5a <- dplyr::bind_cols(obs[,-1], run5a)
filter_lines <-c(59, 72, 99, 237)
run5a <- run5a[!(run5a$LINE %in% filter_lines),]
run5a <- dplyr::tbl_df(run5a)

run5b<- read.csv("170811/170814cont.csv",stringsAsFactors=FALSE)
obs<-read.csv("170811/170814contobs.csv",stringsAsFactors=FALSE)
run5b <- dplyr::bind_cols(obs[,-1], run5b)
filter_lines <-c(295, 321, 386, 490)
run5b <- run5b[!(run5b$LINE %in% filter_lines),]
run5b <- dplyr::tbl_df(run5b)

#run6:
run6 <- read.csv("171207/171207.csv",stringsAsFactors=FALSE)
obs<-read.csv("171207/171207obs.csv",stringsAsFactors=FALSE)
run6 <- dplyr::bind_cols(obs[,-1], run6)
filter_lines <- c(142, 152, 163, 205, 227, 253, 270, 276, 278, 280, 285, 423, 425, 553:582, 668:697)
filter_lines <- c(filter_lines, 191, 195, 206, 279)
run6 <- run6[!(run6$LINE %in% filter_lines),]
run6 <- dplyr::tbl_df(run6)

#run7:
run7 <- read.csv("180209/180209.csv",stringsAsFactors=FALSE)
obs <- obs<-read.csv("180209/180209obs.csv",stringsAsFactors=FALSE)
run7 <- dplyr::bind_cols(obs[,-1], run7)
filter_lines <- c(477:479,831,876,1507:1508,1672,1747:1809,1920)
run7 <- run7[!(run7$LINE %in% filter_lines),]
run7 <- dplyr::tbl_df(run7)


rm(filter_lines)

data<-dplyr::bind_rows(run2, run4, run5a, run5b, run6, run7)

localities <- c(
  "Emmons",
  "Greenbushes",
  "Bamboo Creek", 
  "Trigg Hill", "Spear Hill", "Sifleetes Reward", "Moolyella",
  "Tin Shafts, Poona", "White Lode, Poona",
  "Elsmore", "Aberfoyle",
  "Beechworth",
  "Saltwater Creek", "Blue Tier","Pedra Branca", "Buriti Mine",
  "Renison", "Mt Bischoff" 
  )

data[1:(dim(obs)[2])]<-lapply(data[1:(dim(obs)[2])], as.factor)
data$LOCALITY <- factor(data$LOCALITY, levels=localities)

setwd(wd)
rm(wd, localities, run2, run4, run5a, run5b, run6, run7, obs)
