### check how many waves exist in panel ###
rm(list=ls())
library(tidyverse)

setwd("../data/process_z_data")

panels = c("DHS", "GCOE_Japan", "GCOE_USA", "HILDA", "HRS", "LIKS", "PHF", "SAVE", 
           "SHARE_Austria", "SHARE_Belgium", "SHARE_Czech_Republic", "SHARE_Denmark", "SHARE_Estonia", "SHARE_France", "SHARE_Germany","SHARE_Israel", "SHARE_Italy", "SHARE_Netherlands", "SHARE_Slovenia", "SHARE_Spain", "SHARE_Sweden", "SHARE_Switzerland", 
           "SOEP","USoc")
domains = c("General", "Financial","Driving", "Recreational", "Occupational", "Health", "Social")
ab_domains = c("g", "f", "d", "r", "o", "h", "s")

### function 1: create a empty waves_dataframe for saving waves report
waves = data.frame(matrix(data=NA, nrow=length(panels), ncol = length(domains)))
colnames(waves) = domains
rownames(waves) = panels


### function 2: load data & calculate number of waves
for (i in 1:length(panels)) {
  panelname = panels[i]
  panel = readRDS(paste0(panels[i], "_process_z.Rds")) #load all panel data one by one
  for(j in 1:length(domains)){
    domainname = domains[j]
    domain_name = paste0(domains[j], "_z")
    if(domain_name %in% names(panel)){
      dataset <- panel %>%
        select(ID, age, Gender, SYEAR, domain_name) %>%
        filter(!is.na(get(domain_name)))  %>%
        group_by(ID) %>%
        filter(n()>=2) %>%
        ungroup() %>%
        drop_na() 
      waves[panelname, domainname] = length(unique(dataset$SYEAR))
    }
  }
}


###save results
setwd("../data/results_data")
write.csv(waves, file = "waves.csv")

