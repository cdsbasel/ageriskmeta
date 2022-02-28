###calculate mean age in every sample ###
rm(list=ls())
library(tidyverse)

setwd("../data/process_z_data")


panels = c("DHS", "GCOE_Japan", "GCOE_USA", "HILDA", "HRS", "LIKS", "PHF", "SAVE", 
           "SHARE_Austria", "SHARE_Belgium", "SHARE_Czech_Republic", "SHARE_Denmark", "SHARE_Estonia", "SHARE_France", "SHARE_Germany","SHARE_Israel", "SHARE_Italy", "SHARE_Netherlands", "SHARE_Slovenia", "SHARE_Spain", "SHARE_Sweden", "SHARE_Switzerland", 
           "SOEP","USoc")

### function 1: create a empty age_dataframe for saving waves report
Age_information = data.frame(matrix(data=NA, nrow=length(panels), ncol =4))
rownames(Age_information) = panels
colnames(Age_information) = c("Mean age", "Min age", "Max age", "Age range")

### function 2: load data & calculate mean age
for (i in 1:length(panels)) {
  panelname = panels[i]
  panel = readRDS(paste0(panels[i], "_process_z.Rds")) #load all panel data one by one
  Age_information[panelname, "Mean age"] = mean(panel$age)  
  Age_information[panelname, "Min age"] = min(panel$age)  
  Age_information[panelname, "Max age"] = max(panel$age)  
  Age_information[panelname, "Age range"] = paste0(Age_information[panelname, "Min age"], "-", Age_information[panelname, "Max age"])
}


###save results
setwd("../data/results_data")
saveRDS(Age_information, file = "Age_information.Rds")
write.csv(Age_information, file = "Age_information.csv")