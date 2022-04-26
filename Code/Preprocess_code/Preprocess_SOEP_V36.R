###SOEP Prprocess###
rm(list=ls())
###plh0197-plh202:2004，2009，2014
###plh203: 2004，2009
###plh204: annually 2004 2006 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019

#gender: 1-male  2—female

library(foreign)
library(tidyverse)
library(naniar)

###load data
#if read.spss can not read the file completely, modify terminal command
setwd("../rawdata/SOEP_data/V36")
pldata <- read.spss("pl.sav", use.value.label=FALSE, to.data.frame=TRUE)

###prepare
#select items & caculate
SOEP_process <- pldata  %>%
  dplyr::select(pid, syear, ple0010_h,  pla0009_v1, pla0009_v2,
                plh0204_v1, plh0204_v2,  plh0197, plh0198, plh0199, plh0200, plh0201, plh0202, plh0203) %>%
  dplyr::rename(ID=pid, SYEAR=syear, BIRTH=ple0010_h, Gender=pla0009_v2,
                General=plh0204_v2, Driving=plh0197, Financial=plh0198, Recreational=plh0199, Occupational=plh0200, Health=plh0201, Social=plh0202, Lottery=plh0203) %>%
  dplyr::select(ID, SYEAR, BIRTH, Gender, General, Driving, Financial, Recreational, Occupational, Health, Social) %>%
  dplyr::filter(BIRTH>0) %>%
  mutate(age=SYEAR-BIRTH) %>%
  dplyr::filter(!is.na(age)) %>%
  dplyr::filter(!is.na(Gender)) %>%
  dplyr::select(ID, SYEAR, Gender, age, General, Driving, Financial, Recreational, Occupational, Health, Social)  %>%
  replace_with_na(replace = list(General = c(-8,-5,-1))) %>%
  replace_with_na(replace = list(Driving=c(-8,-1))) %>%
  replace_with_na(replace = list(Financial=c(-8,-1))) %>%
  replace_with_na(replace = list(Recreational=c(-8,-1))) %>%
  replace_with_na(replace = list(Occupational=c(-8,-1))) %>%
  replace_with_na(replace = list(Health=c(-8,-1))) %>%
  replace_with_na(replace = list(Social=c(-8,-1))) 

remove.year.list <- paste(c("1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2005", "2007"), collapse = '|')
SOEP_preprocess <- SOEP_process %>%
  #filter(!grepl(remove.year.list, SYEAR)) %>% #grepl: These functions search for matches of a regular expression/pattern in a character vector. 
  dplyr::filter(age >= 18 & age <= 90) %>%
  filter(!(is.na(General) & is.na(Driving) & is.na(Financial) & is.na(Recreational) & is.na(Occupational) & is.na(Health) & is.na(Social)))


### check Gender and delete participants have different Gender
SOEP_ID_Gender <- SOEP_preprocess %>%
  dplyr::select(ID, Gender)

unique_SOEP_ID_Gender <-  unique(SOEP_ID_Gender) # get unique rows: unique function can eliminate duplicate rows
dupIDs <- with(unique_SOEP_ID_Gender, ID[duplicated(ID)])  # duplicate IDs, which means diffferent Gender: duplicated function can be used to identify duplicate ids after duplicate rows are removed
errorSOEP <- subset(SOEP_preprocess, ID %in% dupIDs)  
rightSOEP <- subset(SOEP_preprocess, !(ID %in% dupIDs)) 

### just keep participants >=2 records
dup_SOEP <- rightSOEP %>%
  dplyr::select(ID, age, Gender, SYEAR) %>%
  filter(!is.na(Gender)) %>%
  group_by(ID) %>%
  filter(n()>=2) %>%
  ungroup()

dup_SOEP_ID <- unique(dup_SOEP$ID)   # Participant ID have duplicate records
SOEP_process1 <- subset(rightSOEP, !(ID %in% dup_SOEP_ID)) # Participant have <2 records
SOEP_process2 <- subset(rightSOEP, (ID %in% dup_SOEP_ID))  # Participant have >=2 records

### cleaned data
SOEP_process <- SOEP_process2


### save 
setwd("../data/process_data")
saveRDS(SOEP_process, file = "SOEP_process.Rds")



### set age group
agebins = c(18, seq(25, 90, 5))
SOEP_process$age_bins <- cut(SOEP_process$age, breaks=agebins, include.lowest = TRUE, right = TRUE, lable = TRUE)

SOEP_process_reference <- SOEP_process %>%
  filter(age_bins=="(50,55]")

mean_General <- mean(SOEP_process_reference$General, na.rm = TRUE)
sd_General <- sd(SOEP_process_reference$General, na.rm = TRUE)

mean_Driving <- mean(SOEP_process_reference$Driving, na.rm = TRUE)
sd_Driving <- sd(SOEP_process_reference$Driving, na.rm = TRUE)

mean_Financial <- mean(SOEP_process_reference$Financial, na.rm = TRUE)
sd_Financial <- sd(SOEP_process_reference$Financial, na.rm = TRUE)

mean_Recreational <- mean(SOEP_process_reference$Recreational, na.rm = TRUE)
sd_Recreational <- sd(SOEP_process_reference$Recreational, na.rm = TRUE)

mean_Occupational <- mean(SOEP_process_reference$Occupational, na.rm = TRUE)
sd_Occupational <- sd(SOEP_process_reference$Occupational, na.rm = TRUE)

mean_Health <- mean(SOEP_process_reference$Health, na.rm = TRUE)
sd_Health <- sd(SOEP_process_reference$Health, na.rm = TRUE)

mean_Social <- mean(SOEP_process_reference$Social, na.rm = TRUE)
sd_Social <- sd(SOEP_process_reference$Social, na.rm = TRUE)

SOEP_process_z <- SOEP_process %>%
  mutate(General_z = (General - mean_General)/sd_General,
         Driving_z = (Driving - mean_Driving)/sd_Driving,
         Financial_z = (Financial - mean_Financial)/sd_Financial,
         Recreational_z = (Recreational - mean_Recreational)/sd_Recreational,
         Occupational_z = (Occupational - mean_Occupational)/sd_Occupational,
         Health_z = (Health - mean_Health)/sd_Health,
         Social_z = (Social - mean_Social)/sd_Social)

### save and load
setwd("../data/process_z_data")
saveRDS(SOEP_process_z, file = "SOEP_process_z.Rds")

#SOEP_process_z <- readRDS(file = "SOEP_process_z.Rds")