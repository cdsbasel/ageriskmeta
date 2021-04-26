###SOEP Prprocess###
rm(list=ls())
###plh0197-plh202:2004，2009，2014
###plh203: 2004，2009
###plh204: annually 2004 2006 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018

#gender: 1-male  2—female

library(foreign)
library(tidyverse)
library(ggplot2)

###load data
#if read.spss can not read the file completely, modify terminal command
setwd("~/Desktop/analysis/meta_analysis_risk_normalized/rawdata/SOEP_data")
pldata <- read.spss("V35/pl.sav", use.value.label=FALSE, to.data.frame=TRUE)

###prepare
#select items & caculate
SOEP_process <- pldata  %>%
  dplyr::select(pid, syear, ple0010_h,  pla0009_v1, pla0009_v2,
                plh0204_v1, plh0204_v2,  plh0197, plh0198, plh0199, plh0200, plh0201, plh0202, plh0203) %>%
  dplyr::rename(ID=pid, SYEAR=syear, BIRTH=ple0010_h, SEX=pla0009_v2,
                General=plh0204_v2, Driving=plh0197, Financial=plh0198, Recreational=plh0199, Occupational=plh0200, Health=plh0201, Social=plh0202, Lottery=plh0203) %>%
  dplyr::select(ID, SYEAR, BIRTH, SEX, General, Driving, Financial, Recreational, Occupational, Health, Social) %>%
  dplyr::filter(BIRTH>0) %>%
  mutate(age=SYEAR-BIRTH) %>%
  dplyr::filter(!is.na(age)) %>%
  dplyr::filter(!is.na(SEX)) %>%
  dplyr::select(ID, SYEAR, SEX, age, General, Driving, Financial, Recreational, Occupational, Health, Social)  %>%
  replace_with_na(replace = list(General = c(-8,-5,-1))) %>%
  replace_with_na(replace = list(Driving=c(-8,-1))) %>%
  replace_with_na(replace = list(Financial=c(-8,-1))) %>%
  replace_with_na(replace = list(Recreational=c(-8,-1))) %>%
  replace_with_na(replace = list(Occupational=c(-8,-1))) %>%
  replace_with_na(replace = list(Health=c(-8,-1))) %>%
  replace_with_na(replace = list(Social=c(-8,-1))) 

remove.year.list <- paste(c("1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2005", "2007"), collapse = '|')
SOEP_process <- SOEP_process %>%
  filter(!grepl(remove.year.list, SYEAR)) #grepl: These functions search for matches of a regular expression/pattern in a character vector. 


### save 
setwd("~/Desktop/analysis/meta_analysis_risk_normalized/data/process_data")
saveRDS(SOEP_process, file = "SOEP_process.Rds")


### set age group
agebins = c(0,15,seq(25,95,10),110)
SOEP_process$age_bins <- cut(SOEP_process$age, breaks=agebins, right = TRUE, lable = TRUE)

SOEP_process_reference <- SOEP_process %>%
  filter(age_bins=="(45,55]")

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
setwd("~/Desktop/analysis/meta_analysis_risk_normalized/data/process_z_data")
saveRDS(SOEP_process_z, file = "SOEP_process_z.Rds")

#SOEP_process_z <- readRDS(file = "SOEP_process_z.Rds")