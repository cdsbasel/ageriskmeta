###SHARE Prprocess###
rm(list=ls())

#age problem??? age should >=50

# SHARE panel include finincial risk aversion and behavioural risk
#variables：
#in cv_r file(demographics data):
#mergeid: Person identifier (fix across modules and waves)
#gender: Male or female    1-male; 2-female
#age_int: Age of respondent at the time of interview
#in as/ex file(risk aversion data)
#as068_(wave 2)
#ex110_(wave 4, 5, 6, 7)
#in br file: including behavioural risk 

#install.packages("tidyverse")
#install.packages("foreign")
#install.packages("ggplot2")
library(foreign)
library(tidyverse)

setwd("../rawdata/SHARE_data/new")

#WAVE 1: there is not risk aversion

# WAVE 2：2006/2007
dems2 <- read.spss("sharew2_rel7-1-0_cv_r.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(mergeid, gender, country, age_int)
risk2 <- read.spss("sharew2_rel7-1-0_as.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(mergeid, country, as068_)
Social2 <- read.spss("sharew2_rel7-1-0_ex.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(mergeid, country, ex026_)
  
data2 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(dems2, risk2, Social2))  %>%
  dplyr::rename(ID=mergeid, Gender=gender, age=age_int, risk_aversion=as068_, Social=ex026_) %>%
  mutate(SYEAR=2007)

 
# WAVE 3:there is not risk aversion, behavioural risk 

#WAVE 4：2011/2012
dems4 <- read.spss("sharew4_rel7-1-0_cv_r.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(mergeid, gender, country, age_int)
risk4 <- read.spss("sharew4_rel7-1-0_ex.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(mergeid, country, ex110_, ex026_)

data4 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(dems4, risk4))  %>%
  dplyr::rename(ID=mergeid, Gender=gender, age=age_int, risk_aversion=ex110_, Social=ex026_) %>%
  mutate(SYEAR=2011)


#WAVE 5: 2013
dems5 <- read.spss("sharew5_rel7-1-0_cv_r.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(mergeid, gender, country, age_int)
risk5 <- read.spss("sharew5_rel7-1-0_ex.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(mergeid, country, ex110_, ex026_)

data5 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(dems5, risk5))  %>%
  dplyr::rename(ID=mergeid, Gender=gender, age=age_int, risk_aversion=ex110_, Social=ex026_) %>%
  mutate(SYEAR=2013)

#WAVE 6:2015/2016
dems6 <- read.spss("sharew6_rel7-1-0_cv_r.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(mergeid, gender, country, age_int)
risk6 <- read.spss("sharew6_rel7-1-0_ex.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(mergeid, country, ex110_, ex026_)

data6 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(dems6, risk6))  %>%
  dplyr::rename(ID=mergeid, Gender=gender, age=age_int,risk_aversion=ex110_,Social=ex026_) %>%
  mutate(SYEAR=2015)

#WAVE 7:2017/2018
dems7 <- read.spss("sharew7_rel7-1-1_cv_r.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(mergeid, gender, country, age_int)
risk7 <- read.spss("sharew7_rel7-1-1_ex.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(mergeid, country, ex110_, ex026_)

data7 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(dems7, risk7))  %>%
  dplyr::rename(ID=mergeid, Gender=gender, age=age_int,risk_aversion=ex110_, Social=ex026_) %>%
  mutate(SYEAR=2017)

#WAVE 8:2019/2020
dems8 <- read.spss("sharew8_rel1-0-0_cv_r.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(mergeid, gender, country, age_int)
risk8 <- read.spss("sharew8_rel1-0-0_ex.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(mergeid, country, ex110_, ex026_)

data8 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(dems8, risk8))  %>%
  dplyr::rename(ID=mergeid, Gender=gender, age=age_int,risk_aversion=ex110_, Social=ex026_) %>%
  mutate(SYEAR=2019)


### combine all waves data
SHARE_data <-rbind(data2, data4, data5, data6, data7, data8) %>%
  dplyr::filter(age>0) 
SHARE_data$risk_aversion[SHARE_data$risk_aversion<0] <- NA

### recode the General variables to be on a 0-10 scale(General is 0-10 already)
SHARE_preprocess <- SHARE_data %>%
  dplyr::filter(!is.na (risk_aversion)) %>%
  rename(Financial_raw=risk_aversion) %>%
  #reverse
  mutate(Financial_reverse = 5-Financial_raw) %>%
  #record to 10 scale
  mutate(Financial=10*(Financial_reverse-1)/(4-1)) %>%
  dplyr::select(ID, SYEAR,  Gender, age, country, Financial_raw, Financial_reverse, Financial) %>% # not include Social
  dplyr::filter(!is.na (age)) %>%
  dplyr::filter(!is.na (Gender)) %>%
  dplyr::filter(age >= 50 & age <= 90) 

Country_all = c("Austria", "Germany", "Sweden", "Netherlands", "Spain", "Italy", "France", "Denmark", "Greece", "Switzerland", "Belgium", 
                "Israel", "Czech_Republic", "Poland", "Ireland", "Luxembourg", "Hungary", "Portugal", "Slovenia", "Estonia", "Croatia",
                "Lithuania","Bulgaria", "Cyprus", "Finland", "Latvia", "Malta", "Romania", "Slovakia") 
Country_all_code <- sort(unique(SHARE_preprocess$country))


setwd("../data/process_data")
agebins = c(18, seq(25, 90, 5))
for(i in 1:length(Country_all)){
  data_select <- SHARE_preprocess %>%
    filter(country==Country_all_code[i]) 
  
  ### check Gender and delete participants have different Gender
  data_select_ID_Gender <- data_select %>%
    dplyr::select(ID, Gender)
  
  unique_data_select_ID_Gender <-  unique(data_select_ID_Gender) # get unique rows: unique function can eliminate duplicate rows
  dupIDs <- with(unique_data_select_ID_Gender, ID[duplicated(ID)])  # duplicate IDs, which means diffferent Gender: duplicated function can be used to identify duplicate ids after duplicate rows are removed
  errordata_select <- subset(data_select, ID %in% dupIDs)  
  rightdata_select <- subset(data_select, !(ID %in% dupIDs))
  
  file_right <- paste0("SHARE_",Country_all[i], "_right")
  assign(file_right, value = rightdata_select)   # Participant have right gender
  
  file_error <- paste0("SHARE_",Country_all[i], "_error")
  assign(file_error, value = errordata_select)   # Participant have error gender
  
  ### just keep participants >=2 records
  dup_data_select<- rightdata_select%>%
    dplyr::select(ID, age, Gender, SYEAR) %>%
    filter(!is.na(Gender)) %>%
    group_by(ID) %>%
    filter(n()>=2) %>%
    ungroup()
  
  dup_data_select_ID <- unique(dup_data_select$ID) # Participant ID have duplicate records
  data_select_process1 <- subset(rightdata_select, !(ID %in% dup_data_select_ID))  # Participant have <2 records
  data_select_process2 <- subset(rightdata_select, (ID %in% dup_data_select_ID))   # Participant have >=2 records
  
  ### cleaned data
  data_name_process1 <- paste0("SHARE_",Country_all[i], "_process1")
  assign(data_name_process1, value = data_select_process1)
  
  data_name_process2 <- paste0("SHARE_",Country_all[i], "_process2")
  assign(data_name_process2, value = data_select_process2)
  
  waves <- length(unique(get(data_name_process2)$SYEAR))
  
 if(waves > 2){  # panel should include at least 3 waves
    data_select_process <- data_select_process2
    data_final <- paste0("SHARE_",Country_all[i], "_process")
    assign(data_final, value = data_select_process)
    
    file_final <- paste0("SHARE_",Country_all[i], "_process.Rds")
    saveRDS(data_select_process, file = file_final)
  }
}







### just keep the country meet screening criteria (14 countries)
Country_select = c("Austria", "Belgium", "Czech_Republic", "Denmark", "Estonia", "France", "Germany",
                   "Israel", "Italy", "Netherlands", "Slovenia", "Spain", "Sweden", "Switzerland")

### set age group
agebins = c(18, seq(25, 90, 5))

setwd("../data/process_z_data")   
for(j in 1:length(Country_select)){
  data_select <- get(paste0("SHARE_",Country_select[j], "_process"))
  data_select$age_bins <- cut(data_select$age, breaks=agebins, include.lowest = TRUE, right = TRUE, lable = TRUE)
  
  
  data_reference <- data_select %>%
    filter(age_bins=="(50,55]")
  mean_Financial <- mean(data_reference$Financial_reverse, na.rm = TRUE)
  sd_Financial <- sd(data_reference$Financial_reverse, na.rm = TRUE)
  
  data_name_process_z <- paste0("SHARE_",Country_select[j], "_process_z")
  data_select_z <- data_select %>%
    mutate(Financial_z = (Financial_reverse - mean_Financial)/sd_Financial)
  assign(data_name_process_z, value = data_select_z)


  file_name <- paste0("SHARE_",Country_select[j], "_process_z.Rds")
  saveRDS(get(data_name_process_z), file = file_name)
}




#Note:
# 1. The almost people in 2019 is new respondents, less participant in previous survey
# 2. note: when analysis, need to delete Greece(country==19) & Poland(country==29) & Luxembourg(country==31), Portugal(country==33) dataset, because less people can be include
