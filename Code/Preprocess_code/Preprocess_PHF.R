###PHF Prprocess###
rm(list=ls())
#variables

#demographic items (P-file:variables that have been collected at the individual level by interviewing all household members aged 16 or older)
# hhid: household id
# impid: inplicate id
# persid: A time-consistent personal identifier (persid) is provided that enables the tracking of specific persons over waves.(persid = hhid * 100 + pid)
# ra0300: age
# dpe9040: gender(1:male  2:female)
# dpe9050: birth year
# intjahr: interview year
# intquartal: interview quarter

#risk items(H-file:variables that have been collected at the household level by interviewing the financially knowledgeable person (FKP))
# zi103: general risk
# hd1800: INVESTMENT ATTITUDES - RISK PREFERENCES-HOUSEHOLD    # should not include in the analysis
# dhd2800: INVESTMENT ATTITUDES - RISK PREFERENCES-INDIVIDUAL  # analysis individual financial risk preference
# If HD1800=5, continue with DHD2800

#install.packages("tidyverse")
#install.packages("foreign")
#install.packages("ggplot2")
library(tidyverse)
library(ggplot2)
library(haven)
library(naniar)

setwd("~/Desktop/analysis/meta_analysis_risk_normalized_5/rawdata/PHF_data")
# wave 1
data_p1<- read_dta("PHF_p_wave1_v3_0.dta")  %>%
  dplyr::filter(pid==impid)  %>%
  dplyr::select(hhid,impid,pid,persid,ra0300,dpe9040)

data_h1<- read_dta("PHF_h_wave1_v3_0.dta") %>%
  dplyr::select(hhid,impid,intjahr,zi103,zi104,hd1800,dhd2800)

data1 <- merge(data_p1, data_h1, by = c("hhid","impid"), all=TRUE) %>%
  mutate(SYEAR=2010)

# wave 2
data_p2<- read_dta("PHF_p_wave2_v3_0.dta") %>%
  dplyr::filter(pid==impid)  %>%
  dplyr::select(hhid,impid,pid,persid,ra0300,dpe9040)

data_h2<- read_dta("PHF_h_wave2_v3_0.dta")  %>%
  dplyr::select(hhid,impid,intjahr,zi103,zi104,hd1800,dhd2800)

data2 <- merge(data_p2, data_h2, by = c("hhid","impid"), all=TRUE)  %>%
  mutate(SYEAR=2014)

# wave 3
data_p3<- read_dta("PHF_p_wave3_v1_0.dta") %>%
  dplyr::filter(pid==impid)  %>%
  dplyr::select(hhid,impid,pid,persid,ra0300,dpe9040)

data_h3<- read_dta("PHF_h_wave3_v1_0.dta") %>%
  dplyr::select(hhid,impid,intjahr,zi103,zi104,hd1800,dhd2800)

data3 <- merge(data_p3, data_h3, by = c("hhid","impid"), all=TRUE) %>%
  mutate(SYEAR=2016)

### combine all waves data
### preprocess and clean data
PHF_preprocess <- rbind(data1, data2, data3) %>%
  rename(ID=persid,age=ra0300, Gender=dpe9040,General=zi103,Social=zi104,Financial_household=hd1800, Financial_raw=dhd2800) %>% 
  mutate(BIRTH=intjahr-age) %>% 
  dplyr::select(ID, SYEAR, Gender, age, General, Financial_raw) %>% # should not include social, include financial 
  replace_with_na(replace = list(General = c(-3,-2, -1))) %>%
  replace_with_na(replace = list(Financial_raw = c(-3,-2, -1))) %>%
  mutate(Financial_reverse = 5-Financial_raw) %>%
  mutate(Financial=10*(Financial_reverse-1)/(4-1)) %>%
  filter(!is.na(Gender)) %>%
  filter(!is.na(age))  %>% 
  dplyr::filter(age >= 18 & age <= 90) %>% 
  filter(!(is.na(General) & is.na(Financial_raw)))



### check Gender and delete participants have different Gender
PHF_ID_Gender <- PHF_preprocess %>%
  dplyr::select(ID, Gender)

unique_PHF_ID_Gender <-  unique(PHF_ID_Gender) # get unique rows: unique function can eliminate duplicate rows
dupIDs <- with(unique_PHF_ID_Gender, ID[duplicated(ID)])  # duplicate IDs, which means diffferent Gender: duplicated function can be used to identify duplicate ids after duplicate rows are removed
errorPHF<- subset(PHF_preprocess, ID %in% dupIDs)  
rightPHF<- subset(PHF_preprocess, !(ID %in% dupIDs)) 

### just keep participants >=2 records
dup_PHF<- rightPHF%>%
  dplyr::select(ID, age, Gender, SYEAR) %>%
  filter(!is.na(Gender)) %>%
  group_by(ID) %>%
  filter(n()>=2) %>%
  ungroup()

dup_PHF_ID <- unique(dup_PHF$ID)
PHF_process1 <- subset(rightPHF, !(ID %in% dup_PHF_ID)) 
PHF_process2 <- subset(rightPHF, (ID %in% dup_PHF_ID)) 

### cleaned data
PHF_process <- PHF_process2


### save 
setwd("~/Desktop/analysis/meta_analysis_risk_normalized_5/data/process_data")
saveRDS(PHF_process, file = "PHF_process.Rds")
  




### set age group
agebins = c(18, seq(25, 90, 5))
PHF_process$age_bins <- cut(PHF_process$age, breaks=agebins, include.lowest = TRUE, right = TRUE, lable = TRUE)

PHF_process_reference <- PHF_process %>%
  filter(age_bins=="(50,55]")

mean_General <- mean(PHF_process_reference$General, na.rm = TRUE)
sd_General <- sd(PHF_process_reference$General, na.rm = TRUE)

mean_Financial <- mean(PHF_process_reference$Financial_reverse, na.rm = TRUE)
sd_Financial <- sd(PHF_process_reference$Financial_reverse, na.rm = TRUE)

PHF_process_z <- PHF_process %>%
  mutate(General_z = (General - mean_General)/sd_General,
         Financial_z = (Financial_reverse - mean_Financial)/sd_Financial)

#save data
setwd("~/Desktop/analysis/meta_analysis_risk_normalized_5/data/process_z_data")
saveRDS(PHF_process_z, file = "PHF_process_z.Rds")

#PHF_process_z <- readRDS(file = "PHF_process_z.Rds")
