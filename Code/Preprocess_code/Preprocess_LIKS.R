###LIKS Prprocess###
rm(list=ls())

#survey year:2010, 2011, 2012, 2013, 2016
# 1--male  2--female

library(foreign)
library(tidyverse)

setwd("../rawdata/LIKS_data")

#2010
dems2010 <- read.spss("2010/hh1a.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(hhid, pid, h102, h103_m, h103_y, age)

risk2010 <- read.spss("2010/id1.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(hhid, pid, i103)

LIKS2010 <- merge(dems2010, risk2010, by = c("hhid", "pid"), all=TRUE) %>%
  rename(Gender=h102, Birth_month=h103_m, Birth_year=h103_y, General=i103) %>%
  mutate(SYEAR=2010)

#2011
dems2011 <- read.spss("2011/hh1a.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(hhid, pid, h102, h103_m, h103_y, age)

risk2011 <- read.spss("2011/id1.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(hhid, pid, i103)

LIKS2011 <- merge(dems2011, risk2011, by = c("hhid", "pid"), all=TRUE) %>%
  rename(Gender=h102, Birth_month=h103_m, Birth_year=h103_y, General=i103) %>%
  mutate(SYEAR=2011)

#2012
dems2012 <- read.spss("2012/hh1a.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(hhid, pid, h102, h103_m, h103_y, age)

risk2012 <- read.spss("2012/id1.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(hhid, pid, i103)

LIKS2012 <- merge(dems2012, risk2012, by = c("hhid", "pid"), all=TRUE) %>%
  rename(Gender=h102, Birth_month=h103_m, Birth_year=h103_y, General=i103) %>%
  mutate(SYEAR=2012)

#2013
dems2013 <- read.spss("2013/hh1a.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(hhid, pid, h102, h103_m, h103_y, h103a)

risk2013 <- read.spss("2013/id1.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(hhid, pid, i103)

LIKS2013 <- merge(dems2013, risk2013, by = c("hhid", "pid"), all=TRUE) %>%
  rename(Gender=h102, Birth_month=h103_m, Birth_year=h103_y, age=h103a, General=i103) %>%
  mutate(SYEAR=2013)

#2016
dems2016 <- read.spss("2016/hh1a.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(hhid, pid, h102, h103m, h103y, h103a)

risk2016 <- read.spss("2016/id1.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  dplyr::select(hhid, pid, i107)

LIKS2016 <- merge(dems2016, risk2016, by = c("hhid", "pid"), all=TRUE) %>%
  rename(Gender=h102, Birth_month=h103m, Birth_year=h103y, age=h103a, General=i107) %>%
  mutate(SYEAR=2016)

### combine all waves data
### preprocess and clean data
LIKS_data <-rbind(LIKS2010, LIKS2011, LIKS2012, LIKS2013, LIKS2016) %>%
  rename(BIRTH = Birth_year)
LIKS_data$ID=paste(LIKS_data$hhid, LIKS_data$pid,sep="_") 

LIKS_preprocess <- LIKS_data %>%
  dplyr::select(ID, Gender, age, SYEAR, General) %>%
  filter(!is.na(Gender)) %>%
  filter(!is.na(General)) %>%
  dplyr::filter(age >= 18 & age <= 90) 

### check Gender and delete participants have different Gender
LIKS_ID_Gender <- LIKS_preprocess %>%
  dplyr::select(ID, Gender)

unique_LIKS_ID_Gender <-  unique(LIKS_ID_Gender) # get unique rows: unique function can eliminate duplicate rows
dupIDs <- with(unique_LIKS_ID_Gender, ID[duplicated(ID)])  # duplicate IDs, which means diffferent Gender: duplicated function can be used to identify duplicate ids after duplicate rows are removed
errorLIKS<- subset(LIKS_preprocess, ID %in% dupIDs)  
rightLIKS<- subset(LIKS_preprocess, !(ID %in% dupIDs)) 

### just keep participants >=2 records
dup_LIKS<- rightLIKS%>%
  dplyr::select(ID, age, Gender, SYEAR) %>%
  filter(!is.na(Gender)) %>%
  group_by(ID) %>%
  filter(n()>=2) %>%
  ungroup()

dup_LIKS_ID <- unique(dup_LIKS$ID)
LIKS_process1 <- subset(rightLIKS, !(ID %in% dup_LIKS_ID)) 
LIKS_process2 <- subset(rightLIKS, (ID %in% dup_LIKS_ID)) 

### cleaned data
LIKS_process <- LIKS_process2




### save 
setwd("../data/process_data")
saveRDS(LIKS_process, file = "LIKS_process.Rds")


### set age group
agebins = c(18, seq(25, 90, 5))
LIKS_process$age_bins <- cut(LIKS_process$age, breaks=agebins, include.lowest = TRUE, right = TRUE, lable = TRUE)

LIKS_process_reference <- LIKS_process %>%
  filter(age_bins=="(50,55]")

mean_General <- mean(LIKS_process_reference$General, na.rm = TRUE)
sd_General <- sd(LIKS_process_reference$General, na.rm = TRUE)

LIKS_process_z <- LIKS_process %>%
  mutate(General_z = (General - mean_General)/sd_General)

# Saving & load XXX object in Rds format
setwd("../data/process_z_data")
saveRDS(LIKS_process_z, file = "LIKS_process_z.Rds")

#LIKS_process_z <- readRDS(file = "LIKS_process_z.Rds")