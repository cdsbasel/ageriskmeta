### GLES Panel 2016-2021 Prprocess ###

rm(list=ls())

### kpx_1572: General risk
# survey time
# W1: kp1_1572, October-November 2016
# A1 Aufstocker: July-August 2017
# W13: April-June 2020
# A2 Profile Wave 2020: September-November 2020
# W14: November-December 2020
# W15: February-March 2021

### ID
# lfdn: Laufende Number (Running number)(Respodent id)
# study: studiennummer (study number)(Study id)

### Demographic
# kpx_2280: Geschlecht (Gender)
# kpx_2290: year or birth  (only find kpx_2290s in W1)
# kpx_2291: month of birth (only find kpx_2290flag and kpx_2291flag, kpx_2290flag == kpx_2291flag)


### Date:
# W1: kp1_datetime
# A1 Aufstocker: kpa1_datetime
# W13: kp13_datetime
# A2 Profile Wave 2020: kpa2_datetime
# W14: kp14_datetime
# W15: kp15_datetime

### load packages
library(foreign)
library(tidyverse)
library(naniar)

path = "../rawdata/GLES_data/sav"   # sav. format dataset does not include a1, but there is something wrong about the csv. format file;
setwd(path)

W1 <- read.spss("ZA6838_w1to9_sA_v5-0-0.sav", to.data.frame = T, use.value.labels = F, reencode=F)  %>%
  select(study, lfdn, kpx_2280, kpx_2290s, kpx_2291flag, kp1_1572, kp1_datetime) %>%
  rename(ID=lfdn, Gender=kpx_2280, birth_yr=kpx_2290s, birth_mo=kpx_2291flag, intdate=kp1_datetime, risk=kp1_1572) %>%
  mutate(birth_date = as.Date(paste(birth_mo, 15, birth_yr, sep="/"), "%m/%d/%Y")) %>%
  mutate(int_date = as.Date(intdate,format='%Y-%m-%d')) %>%
  mutate(age = round((as.numeric(int_date - birth_date))/365)) %>%
  mutate(SYEAR = 1)
  


W13 <- read.spss("ZA6838_w13_sA_v5-0-0.sav", to.data.frame = T, use.value.labels = F, reencode=F) %>%
  select(study, lfdn, kpx_2280, kpx_2290s, kpx_2291flag, kp13_1572, kp13_datetime) %>%
  rename(ID=lfdn, Gender=kpx_2280, birth_yr=kpx_2290s, birth_mo=kpx_2291flag, intdate=kp13_datetime, risk=kp13_1572) %>%
  mutate(birth_date = as.Date(paste(birth_mo, 15, birth_yr, sep="/"), "%m/%d/%Y")) %>%
  mutate(int_date = as.Date(intdate,format='%Y-%m-%d')) %>%
  mutate(age = round((as.numeric(int_date - birth_date))/365)) %>%
  mutate(SYEAR = 13)
  


W14 <- read.spss("ZA6838_w14_sA_v5-0-0.sav", to.data.frame = T, use.value.labels = F, reencode=F) %>%
  select(study, lfdn, kpx_2280, kpx_2290s, kpx_2291flag, kp14_1572, kp14_datetime) %>%
  rename(ID=lfdn, Gender=kpx_2280, birth_yr=kpx_2290s, birth_mo=kpx_2291flag, intdate=kp14_datetime, risk=kp14_1572) %>%
  mutate(birth_date = as.Date(paste(birth_mo, 15, birth_yr, sep="/"), "%m/%d/%Y")) %>%
  mutate(int_date = as.Date(intdate,format='%Y-%m-%d')) %>%
  mutate(age = round((as.numeric(int_date - birth_date))/365)) %>%
  mutate(SYEAR = 14)
  

W15 <- read.spss("ZA6838_w15_sA_v5-0-0.sav", to.data.frame = T, use.value.labels = F, reencode=F) %>%
  select(study, lfdn, kpx_2280, kpx_2290s, kpx_2291flag, kp15_1572, kp15_datetime) %>%
  rename(ID=lfdn, Gender=kpx_2280, birth_yr=kpx_2290s, birth_mo=kpx_2291flag, intdate=kp15_datetime, risk=kp15_1572) %>%
  mutate(birth_date = as.Date(paste(birth_mo, 15, birth_yr, sep="/"), "%m/%d/%Y")) %>%
  mutate(int_date = as.Date(intdate,format='%Y-%m-%d')) %>%
  mutate(age = round((as.numeric(int_date - birth_date))/365)) %>%
  mutate(SYEAR = 15)
  


GLES_preprocess <- Reduce(function(x, y) merge(x, y, all=TRUE), list(W1, W13, W14, W15)) %>%
  select(ID, SYEAR, Gender, age, risk) %>%
  rename(General_raw = risk) %>%
  replace_with_na(replace = list(General_raw = c(-93, -95, -99))) %>%
  mutate(General=10*(General_raw-1)/(11-1)) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(Gender)) %>%
  filter(age >= 18 & age <= 90) 
  


### check Gender and delete participants have different Gender
GLES_ID_Gender <- GLES_preprocess %>%
  dplyr::select(ID, Gender)

unique_GLES_ID_Gender <-  unique(GLES_ID_Gender) # get unique rows: unique function can eliminate duplicate rows
dupIDs <- with(unique_GLES_ID_Gender, ID[duplicated(ID)])  # duplicate IDs, which means different Gender: duplicated function can be used to identify duplicate ids after duplicate rows are removed
errorGLES <- subset(GLES_preprocess, ID %in% dupIDs)  
rightGLES <- subset(GLES_preprocess, !(ID %in% dupIDs)) 

### just keep participants >=2 records
dup_GLES <- rightGLES %>%
  dplyr::select(ID, age, Gender, SYEAR) %>%
  filter(!is.na(Gender)) %>%
  group_by(ID) %>%
  filter(n()>=2) %>%
  ungroup()

dup_GLES_ID <- unique(dup_GLES$ID)   # Participant ID have duplicate records
GLES_process1 <- subset(rightGLES, !(ID %in% dup_GLES_ID)) # Participant have <2 records
GLES_process2 <- subset(rightGLES, (ID %in% dup_GLES_ID))  # Participant have >=2 records

### cleaned data
GLES_process <- GLES_process2


### save 
setwd("../data/process_data")
saveRDS(GLES_process, file = "GLES_process.Rds")



### set age group
agebins = c(18, seq(25, 90, 5))
GLES_process$age_bins <- cut(GLES_process$age, breaks=agebins, include.lowest = TRUE, right = TRUE, lable = TRUE)

GLES_process_reference <- GLES_process %>%
  filter(age_bins=="(50,55]")

mean_General <- mean(GLES_process_reference$General, na.rm = TRUE)
sd_General <- sd(GLES_process_reference$General, na.rm = TRUE)

GLES_process_z <- GLES_process %>%
  mutate(General_z = (General - mean_General)/sd_General)

### save and load
setwd("../data/process_z_data")
saveRDS(GLES_process_z, file = "GLES_process_z.Rds")

#GLES_process_z <- readRDS(file = "GLES_process_z.Rds")

  

