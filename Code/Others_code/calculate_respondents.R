### calculate how many respondents exist in each panel ###
rm(list=ls())
library(tidyverse)

setwd("../data/process_z_data")

panels = c("DHS", "GCOE_Japan", "GCOE_USA", "HILDA", "HRS", "LIKS", "PHF", "SAVE", 
           "SHARE_Austria", "SHARE_Belgium", "SHARE_Czech_Republic", "SHARE_Denmark", "SHARE_Estonia", "SHARE_France", "SHARE_Germany","SHARE_Israel", "SHARE_Italy", "SHARE_Netherlands", "SHARE_Slovenia", "SHARE_Spain", "SHARE_Sweden", "SHARE_Switzerland", 
           "SOEP","USoc")
domains = c("General", "Financial","Driving", "Recreational", "Occupational", "Health", "Social")
ab_domains = c("g", "f", "d", "r", "o", "h", "s")

### function 1: create a empty respondents_dataframe for saving number of respondents 
respondents = data.frame(matrix(data=NA, nrow=length(panels), ncol = length(domains)))
colnames(respondents) = domains
rownames(respondents) = panels


### function 2: load data & calculate number of respondents for every domain
for (i in 1:length(panels)) {
  panelname = panels[i]
  panel = readRDS(paste0(panels[i], "_process_z.Rds")) #load all panel data one by one
  dataset = panel %>%
    select(ID, age, Gender, SYEAR) %>%
    group_by(ID) %>%
    filter(n()>=2) %>%
    ungroup() 
 
  for(j in 1:length(domains)){
    domainname = domains[j]
    domain_name = paste0(domains[j], "_z")
    if(domain_name %in% names(panel)){
      dataset = panel %>%
        select(ID, age, Gender, SYEAR, domain_name) %>%
        filter(!is.na(get(domain_name)))  %>%
        group_by(ID) %>%
        filter(n()>=2) %>%
        ungroup() 
      respondents[panelname, domainname] = length(unique(dataset$ID))
    }
  }
}
  
### function 3: select maximum value for every panel
for (x in 1:nrow(respondents)) {
  respondents[x, "max"] = respondents[x, which.max(respondents[x,])]
}

### function 4: load data & calculate number of male/female respondents in each panel
for (i in 1:length(panels)) {
  panelname = panels[i]
  panel = readRDS(paste0(panels[i], "_process_z.Rds")) #load all panel data one by one
  dataset <- panel %>%
    select(ID, age, Gender, SYEAR) %>%
    filter(!is.na(Gender))  %>%
    group_by(ID) %>%
    filter(n()>=2) %>%
    ungroup() 
  
  respondents[panelname, "total"] = length(unique(dataset$ID))
  
  dataset_male <- dataset %>%
    filter(Gender == 1) 
  respondents[panelname, "Male"] = length(unique(dataset_male$ID))
  
  dataset_female <- dataset %>%
    filter(Gender == 2)
  respondents[panelname, "Female"] = length(unique(dataset_female$ID))
  
  respondents[panelname, "Male_percent"] = (respondents[panelname, "Male"])/respondents[panelname, "total"]
  respondents[panelname, "Female_percent"] = (respondents[panelname, "Female"])/respondents[panelname, "total"]
  
  respondents[panelname, "sumgender"] = respondents[panelname, "Male"] + respondents[panelname, "Female"]
  respondents[panelname, "check"] = respondents[panelname, "total"] - respondents[panelname, "sumgender"]
  }


sum_respondents = respondents
sum_respondents["sum","total"] = sum(respondents$total, na.rm = TRUE)
sum_respondents["sum","max"] = sum(respondents$max, na.rm = TRUE)
sum_respondents["sum","General"] = sum(respondents$General, na.rm = TRUE)
sum_respondents["sum","Driving"] = sum(respondents$Driving, na.rm = TRUE)
sum_respondents["sum","Financial"] = sum(respondents$Financial, na.rm = TRUE)
sum_respondents["sum","Recreational"] = sum(respondents$Recreational, na.rm = TRUE)
sum_respondents["sum","Occupational"] = sum(respondents$Occupational, na.rm = TRUE)
sum_respondents["sum","Health"] = sum(respondents$Health, na.rm = TRUE)
sum_respondents["sum","Social"] = sum(respondents$Social, na.rm = TRUE)
sum_respondents["sum","Male"] = sum(respondents$Male, na.rm = TRUE)
sum_respondents["sum","Female"] = sum(respondents$Female, na.rm = TRUE)
sum_respondents["sum", "sumgender"] <- sum(respondents$sumgender, na.rm = TRUE)
sum_respondents["sum", "check"] <- sum(respondents$check, na.rm = TRUE)

sum_respondents["sum", "Male_percent"] = sum_respondents["sum", "Male"]/sum_respondents["sum", "total"]
sum_respondents["sum", "Female_percent"] = sum_respondents["sum", "Female"]/sum_respondents["sum", "total"]


###save results
setwd("../data/results_data")
saveRDS(sum_respondents, file = "sum_respondents.Rds")
write.csv(sum_respondents, file = "sum_respondents.csv")





