#LSM_Tables
library(dplyr)
library(xtable)
library(openxlsx)

source("Model.R")

ions <- read.xlsx("ionic_radii.xlsx", sheet="trace_all")
ions <- filter(ions, spin=="H"|is.na(spin)==TRUE)
ions <- ions[order(ions$radius),]
ions <- data.frame(cbind(ions, D=signif(Di(ions$radius, ions$charge, 800),2)))
colnames(ions)[c(1,8,12)]<-c("Element","IR","DM/Sn")

#for exporting to excel
Fe2 <- 0.78
Fe3 <- 0.645
Nb5 <- 0.64
W6  <- 0.6
ionsList <- split(ions[,c(1,8,12)], f = ions$charge)
#Calculate D's for coupled substitutions
ionsList$'2' <- cbind(ionsList$`2`,
                      MNb2O6=signif(Di((ionsList$`2`$IR+Nb5*2)/3, 4, 800),2),
                      MWO4=signif(Di((ionsList$`2`$IR+W6)/2, 4, 800),2))
ionsList$'3' <- cbind(ionsList$`3`,
                      MOOH=signif(Di(ionsList$`3`$IR, 4, 800),2),
                      MNbO2=signif(Di((ionsList$`3`$IR+Nb5)/2, 4, 800),2))
ionsList$'5' <- cbind(ionsList$`5`,
                      FeM2O6=signif(Di((ionsList$`5`$IR*2+Fe2)/3, 4, 800),2),
                      FeMO2=signif(Di((ionsList$`5`$IR+Fe3)/2, 4, 800),2))
ionsList$'6' <- cbind(ionsList$`6`,
                      FeMO2=signif(Di((ionsList$`6`$IR+Fe2)/2, 4, 800),2))
write.xlsx(ionsList, "LSM_summary.xlsx")

#for exporting to latex
ionsList <- lapply(ionsList, xtable)
captext <- "Results of the lattice strain model for "
shorttitle <- "Lattice strain model results: "
longcap <- "Ionic radii (IR) are listed for 6-fold coordination, and high spin (where relevant) from \\ref{Shannon1967}. The calculated relative partition coefficient D\\textsubscript{M/Sn} is for the 'pure' strain model. "
add1 <- "No charge balanced or coupled substitutions are considered for M\\plus cations."
add2 <- "Charge and strain compensation is considered via two coupled substitutions for M\\two cations, M\\two{}Nb\\five{}\\textsubscript{2} (MNb\\textsubscript{2}O\\textsubscript{6}) and M\\two{}W\\six{} (MWO\\textsubscript{4})."
add3 <- "Charge balance for M\\three cations is possible via protonation of a nearby oxygen (MO.OH), and a coupled substitution of the form M\\three{}Nb\\five{} (MNbO\\textsubscript{4}) is also considered."
add4 <- "Only the pure lattice strain model applies to M\\four cations."
add5 <- "Two coupled substitutions are also considered for M\\five cations, Fe\\two{}M\\five{}\\textsubscript{2} (FeM\\textsubscript{2}O\\textsubscript{6}) and Fe\\three{}M\\five{} (FeMO\\textsubscript{4})."
add6 <- "Only one coupled substition mechanism is considered for M\\six cations, Fe\\two{}M\\six{} (FeMO\\textsubscript{4})."

caption(ionsList$'1')<- c(paste0(captext, "monovalent cations. ", longcap, add1), paste0(shorttitle, "monovalent cations"))
label(ionsList$'1') <- paste0("tab:LSM1")
display(ionsList$'1') <- c("s","s","g","g")
colnames(ionsList$'1') <- c("","IR (\\r{A})", "D\\textsubscript{M/Sn}")

attr(ionsList$'2',"caption") <- c(paste0(captext,"divalent cations. ", longcap, add2),paste0(shorttitle, "divalent cations"))
label(ionsList$'2') <- paste0("tab:LSM2")
display(ionsList$'2') <- c("s","s","g","g","g","g")
colnames(ionsList$'2') <- c("","IR (\\r{A})", "D\\textsubscript{M/Sn}", "MNb\\textsubscript{2}O\\textsubscript{6}", "MWO\\textsubscript{4}")

attr(ionsList$'3',"caption") <- c(paste0(captext,"trivalent cations. ", longcap, add3),paste0(shorttitle, "trivalent cations"))
label(ionsList$'3') <- paste0("tab:LSM3")
display(ionsList$'3') <- c("s","s","g","g","g","g")
colnames(ionsList$'3') <- c("","IR (\\r{A})", "D\\textsubscript{M/Sn}", "MO.OH", "MNbO\\textsubscript{4}")

attr(ionsList$'4',"caption") <- c(paste0(captext,"tetravalent cations. ", longcap, add4),paste0(shorttitle, "tetravalent cations"))
label(ionsList$'4') <- paste0("tab:LSM4")
display(ionsList$'4') <- c("s","s","g","g")
colnames(ionsList$'4') <- c("","IR (\\r{A})", "D\\textsubscript{M/Sn}")

attr(ionsList$'5',"caption") <- c(paste0(captext,"pentavalent cations. ", longcap, add5),paste0(shorttitle, "pentavalent cations"))
label(ionsList$'5') <- paste0("tab:LSM5")
display(ionsList$'5') <- c("s","s","g","g","g","g")
colnames(ionsList$'5') <- c("","IR (\\r{A})", "D\\textsubscript{M/Sn}", "FeM\\textsubscript{2}O\\textsubscript{6}", "FeMO\\textsubscript{4}")

attr(ionsList$'6',"caption") <- c(paste0(captext,"hexavalent cations. ", longcap, add6),paste0(shorttitle, "hexavalent cations"))
label(ionsList$'6') <- paste0("tab:LSM6")
display(ionsList$'6') <- c("s","s","g","g","g")
colnames(ionsList$'6') <- c("","IR (\\r{A})", "D\\textsubscript{M/Sn}", "FeMO\\textsubscript{4}")

if(file.exists("LSM_summary.tex")){file.remove("LSM_summary.tex")}

for(table in ionsList){
print(table,  
      type="latex",
      file="LSM_summary.tex",
      append = TRUE,
      floating = TRUE,
      include.rownames = FALSE,
      size="small",
      tabular.environment = "tabular",
      math.style.exponents = TRUE,
      math.style.negative = TRUE,
      colnames.format = "multiple",
      booktabs = TRUE,
      NA.string = "-",
      sanitize.text.function = function(x) {x},
      print.results=TRUE)
}