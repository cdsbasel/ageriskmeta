### calculate how many respondents exist in each panel ###
rm(list=ls())
library(tidyverse)
setwd("/Users/yliu/Desktop/analysis/meta_analysis_risk_normalized_5/data/process_z_data")

# do not include Addhealth / NLSY79_Child / NLSY79_YA panel
panels <- c("DHS","GCOE_Japan","GCOE_USA","GLES","HILDA","HRS","LIKS","PHF","SAVE","SHARE_Austria","SHARE_Belgium","SHARE_Czech_Republic","SHARE_Denmark", "SHARE_Estonia", "SHARE_France", "SHARE_Germany","SHARE_Israel", "SHARE_Italy","SHARE_Netherlands","SHARE_Slovenia","SHARE_Spain","SHARE_Sweden","SHARE_Switzerland","SOEP","UAS","USoc")   # do not include Addhealth / NLSY79_Child / NLSY79_YA panel
domains <- c("General", "Financial","Driving", "Recreational", "Occupational", "Health", "Social")
ab_domains <- c("g", "f", "d", "r", "o", "h", "s")

### function 1: create a empty respondents_dataframe for saving number of respondents; create a empty meanage dataframe for saving mean age; 
respondents <- data.frame(matrix(data=NA, nrow=length(panels), ncol = length(domains)))
colnames(respondents) <- domains
rownames(respondents) <- panels

meanage <- data.frame(matrix(data=NA, nrow=length(panels), ncol = length(domains)))
colnames(meanage) <- domains
rownames(meanage) <- panels


### function 2: load data & calculate number of respondents for every domain
for (i in 1:length(panels)) {
  panelname <- panels[i]
  print(panelname)
  panel <- readRDS(paste0(panels[i], "_process_z.Rds")) #load all panel data one by one
  dataset <- panel %>%
    dplyr::select(ID, age, Gender, SYEAR) %>%
    group_by(ID) %>%
    filter(n()>=2) %>%
    ungroup() 
 
  for(j in 1:length(domains)){
    domainname <- domains[j]
    print(domainname)
    domain_name <- paste0(domains[j], "_z")
    print(domain_name)
    if(domain_name %in% names(panel)){
      dataset <- panel %>%
        dplyr::select(ID, age, Gender, SYEAR, domain_name) %>%
        filter(!is.na(get(domain_name)))  %>%
        group_by(ID) %>%
        filter(n()>=2) %>%
        ungroup() 
      respondents[panelname, domainname] <- length(unique(dataset$ID))
      meanage[panelname, domainname] <- mean(dataset$age)
    }
  }
}
  
### function 3: select maximum value for every panel
for (x in 1:nrow(respondents)) {
  respondents[x, "max"] <- respondents[x, which.max(respondents[x,])]
}

### calculate how many male/female respondents exist in each panel ###
### function 4: load data & calculate number of male/female respondents
setwd("/Users/yliu/Desktop/analysis/meta_analysis_risk_normalized_5/data/process_z_data")
  
for (i in 1:length(panels)) {
  panelname <- panels[i]
  print(panelname)
  panel <- readRDS(paste0(panels[i], "_process_z.Rds")) #load all panel data one by one
  dataset <- panel %>%
    dplyr::select(ID, age, Gender, SYEAR) %>%
    filter(!is.na(Gender))  %>%
    group_by(ID) %>%
    filter(n()>=2) %>%
    ungroup() 
  
  respondents[panelname, "total"] <- length(unique(dataset$ID))
  meanage[panelname, "total"] <- mean(dataset$age)
  
  dataset_male <- dataset %>%
    dplyr::filter(Gender==1) 
  respondents[panelname, "Male"] <- length(unique(dataset_male$ID))
  
  dataset_female <- dataset %>%
    dplyr::filter(Gender==2) 
  respondents[panelname, "Female"] <- length(unique(dataset_female$ID))
  
  respondents[panelname, "Male_percent"] <- (respondents[panelname, "Male"])/respondents[panelname, "total"]
  respondents[panelname, "Female_percent"] <- (respondents[panelname, "Female"])/respondents[panelname, "total"]
  
  respondents[panelname, "sumgender"] <- respondents[panelname, "Male"] + respondents[panelname, "Female"]
  respondents[panelname, "check"] <- respondents[panelname, "total"] - respondents[panelname, "sumgender"]
  }


sum_respondents <- respondents
sum_respondents["sum","total"] <- sum(respondents$total, na.rm = TRUE)
sum_respondents["sum","max"] <- sum(respondents$max, na.rm = TRUE)
sum_respondents["sum","General"] <- sum(respondents$General, na.rm = TRUE)
sum_respondents["sum","Driving"] <- sum(respondents$Driving, na.rm = TRUE)
sum_respondents["sum","Financial"] <- sum(respondents$Financial, na.rm = TRUE)
sum_respondents["sum","Recreational"] <- sum(respondents$Recreational, na.rm = TRUE)
sum_respondents["sum","Occupational"] <- sum(respondents$Occupational, na.rm = TRUE)
sum_respondents["sum","Health"] <- sum(respondents$Health, na.rm = TRUE)
sum_respondents["sum","Social"] <- sum(respondents$Social, na.rm = TRUE)
sum_respondents["sum","Male"] <- sum(respondents$Male, na.rm = TRUE)
sum_respondents["sum","Female"] <- sum(respondents$Female, na.rm = TRUE)
sum_respondents["sum", "sumgender"] <- sum(respondents$sumgender, na.rm = TRUE)
sum_respondents["sum", "check"] <- sum(respondents$check, na.rm = TRUE)

sum_respondents["sum", "Male_percent"] <- (sum_respondents["sum", "Male"])/sum_respondents["sum", "total"]
sum_respondents["sum", "Female_percent"] <- (sum_respondents["sum", "Female"])/sum_respondents["sum", "total"]


###save results
setwd("/Users/yliu/Desktop/analysis/meta_analysis_risk_normalized_5/data")
saveRDS(sum_respondents, file = "results_data/sum_respondents.Rds")
write.csv(sum_respondents, file = "results_data/sum_respondents.csv")


saveRDS(respondents, file = "results_data/respondents.Rds")
write.csv(respondents, file = "results_data/respondents.csv")

saveRDS(meanage, file = "results_data/meanage.Rds")
write.csv(meanage, file = "results_data/meanage.csv")




