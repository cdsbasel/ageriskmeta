###Preprocess_USoc###
rm(list=ls())
# Understanding society study including main study(6614) & innovation panel(IP, 6649), but we just found more than 3 wave of risk-taking in innovation panel.
# wave 1: scriska(prepared to take risks);  scriskb(prepared to risk trusting strangers)
# wave 6: trriska(prepared to take risks);  trhlrisk(prepared to take health risks);  trflrisk(prepared to take financial risks)
# wave 7: trriska(prepared to take risks);  trhlrisk(prepared to take health risks);  trflrisk(prepared to take financial risks)
# so, in this research, we just analysis general risk taking in wave 1(scriska), 6, 7(trriska) in innovation panel(IP) study.
# data file: indresp_ip

library(tidyverse)
library(foreign)

setwd("../rawdata/USoc_data")

#wave1(2008)
wave1 <- read.spss("Innovation panel_6849/a_indresp_ip.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  select(pidp, a_istrtdatd, a_istrtdatm, a_istrtdaty, a_sex, a_birthy, a_dvage, a_scriska) %>%
  rename(ID=pidp, interviewday=a_istrtdatd, interviewmonth=a_istrtdatm, SYEAR=a_istrtdaty, Gender=a_sex, BIRTH=a_birthy, age=a_dvage, General=a_scriska) %>%
  filter(General>=0)

#wave6(2013)
wave6 <- read.spss("Innovation panel_6849/f_indresp_ip.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  select(pidp, f_istrtdatd, f_istrtdatm, f_istrtdaty, f_sex, f_birthy, f_dvage, f_trriska) %>%
  rename(ID=pidp, interviewday=f_istrtdatd, interviewmonth=f_istrtdatm, SYEAR=f_istrtdaty, Gender=f_sex, BIRTH=f_birthy, age=f_dvage, General=f_trriska) %>%
  filter(General>=0)   # there are many missing or inapplicable in wave 6
  

#wave7(2014)
wave7 <- read.spss("Innovation panel_6849/g_indresp_ip.sav", use.value.label=FALSE, to.data.frame=TRUE) %>%
  select(pidp, g_istrtdatd, g_istrtdatm, g_istrtdaty, g_sex, g_birthy, g_dvage, g_trriska) %>%
  rename(ID=pidp, interviewday=g_istrtdatd, interviewmonth=g_istrtdatm, SYEAR=g_istrtdaty, Gender=g_sex, BIRTH=g_birthy, age=g_dvage, General=g_trriska) %>%
  filter(General>=0)  %>% # there are many missing or inapplicable in wave 7
  mutate(General=10-General)   ##############note: wave 7 data should be reverse
  

### ### combine all waves data
### preprocess and clean data
USoc_preprocess <- Reduce(function(x, y) merge(x, y, all=TRUE), list(wave1, wave6, wave7)) %>%
  select(ID, SYEAR, Gender, age, General) %>% 
  filter(!is.na(General)) %>%
  filter(!is.na(Gender)) %>%
  filter(!is.na(age)) %>%
  dplyr::filter(age >= 18 & age <= 90) 

### check Gender and delete participants have different Gender
USoc_ID_Gender <- USoc_preprocess %>%
  dplyr::select(ID, Gender)

unique_USoc_ID_Gender <-  unique(USoc_ID_Gender) # get unique rows: unique function can eliminate duplicate rows
dupIDs <- with(unique_USoc_ID_Gender, ID[duplicated(ID)])  # duplicate IDs, which means diffferent Gender: duplicated function can be used to identify duplicate ids after duplicate rows are removed
errorUSoc <- subset(USoc_preprocess, ID %in% dupIDs)  
rightUSoc <- subset(USoc_preprocess, !(ID %in% dupIDs)) 

### just keep participants >=2 records
dup_USoc <- rightUSoc %>%
  dplyr::select(ID, age, Gender, SYEAR) %>%
  filter(!is.na(Gender)) %>%
  group_by(ID) %>%
  filter(n()>=2) %>%
  ungroup()

dup_USoc_ID <- unique(dup_USoc$ID)   # Participant ID have duplicate records
USoc_process1 <- subset(rightUSoc, !(ID %in% dup_USoc_ID)) # Participant have <2 records
USoc_process2 <- subset(rightUSoc, (ID %in% dup_USoc_ID))  # Participant have >=2 records

### cleaned data
USoc_process <- USoc_process2





### save 
setwd("../data/process_data")
saveRDS(USoc_process, file = "USoc_process.Rds")


### set age group
agebins = c(18, seq(25, 90, 5))
USoc_process$age_bins <- cut(USoc_process$age, breaks=agebins, include.lowest = TRUE, right = TRUE, lable = TRUE)

USoc_process_reference <- USoc_process %>%
  filter(age_bins=="(50,55]")

mean_General <- mean(USoc_process_reference$General, na.rm = TRUE)
sd_General <- sd(USoc_process_reference$General, na.rm = TRUE)

USoc_process_z <- USoc_process %>%
  mutate(General_z = (General - mean_General)/sd_General)
  

### save and load
setwd("../data/process_z_data")
saveRDS(USoc_process_z, file = "USoc_process_z.Rds")
# USoc_process_z <- readRDS(file = "USoc_process_z.Rds")

 