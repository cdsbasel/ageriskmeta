### UAS Prprocess ###
rm(list=ls())

### load package
library(tidyverse)
library(haven)

### set working directory
path = "~/Desktop/analysis/meta_analysis_risk_normalized_5/rawdata/UAS_data/General_dta"   
setwd(path)

### load dataset
# UAS 20---HRS WAVE 1
UAS20 <- read_dta('uas20.dta', encoding = "latin1") %>%
  select(uashhid, uasid, gender, dateofbirth_year, age, agerange, b132_) %>%   #gender: 0=Female, 1=Male
  mutate(SYEAR = 2015) 

# UAS 95---HRS WAVE 2
UAS95 <- read_dta('uas95.dta', encoding = "latin1") %>%
  select(uashhid, uasid, gender, dateofbirth_year, age, agerange, b132_) %>%
  mutate(SYEAR = 2017)

# UAS 185---HRS WAVE 3
UAS185 <- read_dta('uas185.dta', encoding = "latin1") %>%
  select(uashhid, uasid, gender, dateofbirth_year, age, agerange, b132_) %>%
  mutate(SYEAR = 2019)

# UAS 396---HRS WAVE 4
UAS396 <- read_dta('uas396.dta', encoding = "latin1") %>%
  select(uashhid, uasid, gender, dateofbirth_year, age, agerange, b132_) %>%
  mutate(SYEAR = 2021)


### Merge data
UAS_preprocess <- Reduce(function(x, y) merge(x, y, all=TRUE), list(UAS20, UAS95, UAS185, UAS396)) %>%
  select(uasid, gender, age, b132_, SYEAR) %>%
  rename(ID=uasid, Gender=gender,  General = b132_) %>%
  mutate(Gender = replace(Gender, Gender==0, 2)) %>%  #change gender value to: 2=Female, 1=Male
  mutate(Gender = as.numeric(Gender)) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(Gender)) %>%
  filter(age >= 18 & age <= 90) 
  

### check Gender and delete participants have different Gender
UAS_ID_Gender <- UAS_preprocess %>%
  dplyr::select(ID, Gender)

unique_UAS_ID_Gender <-  unique(UAS_ID_Gender) # get unique rows: unique function can eliminate duplicate rows
dupIDs <- with(unique_UAS_ID_Gender, ID[duplicated(ID)])  # duplicate IDs, which means different Gender: duplicated function can be used to identify duplicate ids after duplicate rows are removed
errorUAS <- subset(UAS_preprocess, ID %in% dupIDs)  
rightUAS <- subset(UAS_preprocess, !(ID %in% dupIDs)) 

### just keep participants >=2 records
dup_UAS <- rightUAS %>%
  dplyr::select(ID, age, Gender, SYEAR) %>%
  filter(!is.na(Gender)) %>%
  group_by(ID) %>%
  filter(n()>=2) %>%
  ungroup()

dup_UAS_ID <- unique(dup_UAS$ID)   # Participant ID have duplicate records
UAS_process1 <- subset(rightUAS, !(ID %in% dup_UAS_ID)) # Participant have <2 records
UAS_process2 <- subset(rightUAS, (ID %in% dup_UAS_ID))  # Participant have >=2 records

### cleaned data
UAS_process <- UAS_process2


### save 
setwd("~/Desktop/analysis/meta_analysis_risk_normalized_5/data/process_data")
saveRDS(UAS_process, file = "UAS_process.Rds")



### set age group
agebins = c(18, seq(25, 90, 5))
UAS_process$age_bins <- cut(UAS_process$age, breaks=agebins, include.lowest = TRUE, right = TRUE, lable = TRUE)

UAS_process_reference <- UAS_process %>%
  filter(age_bins=="(50,55]")

mean_General <- mean(UAS_process_reference$General, na.rm = TRUE)
sd_General <- sd(UAS_process_reference$General, na.rm = TRUE)

UAS_process_z <- UAS_process %>%
  mutate(General_z = (General - mean_General)/sd_General)

### save and load
setwd("~/Desktop/analysis/meta_analysis_risk_normalized_5/data/process_z_data")
saveRDS(UAS_process_z, file = "UAS_process_z.Rds")

#UAS_process_z <- readRDS(file = "UAS_process_z.Rds")