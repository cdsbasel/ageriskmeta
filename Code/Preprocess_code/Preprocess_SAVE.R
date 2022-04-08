###SAVE Prprocess###
rm(list=ls())

library(foreign)
library(tidyverse)
library(polycor)

# PATH: this path indicates the location of the "data" folder (and should be adapted if location changes)
setwd("~/Desktop/analysis/meta_analysis_risk_normalized_5/rawdata/SAVE_data")

removeimp=function(data,indicators,vars_imp,vars_ind,labels)
{
  for (counter in 1:length(vars_imp))
  {
    data[indicators[,vars_ind[counter]]==1,vars_imp[counter]]=NA
  }
  names(data)=labels
  data$age=data$year-(data$birthyr+1900)
  return(data)
}


vars_imp=c("respid","f06s_imp","f07o_imp","f98bg1_imp","f98bg2_imp","f98bg3_imp","f98bg4_imp","f98bg5_imp","f99g1_imp", "f99g2_imp", "f99g3_imp", "f99g4_imp","fg4s1_imp","f52g3_imp", "f99fg2_imp","year")
vars_ind=c("respid","f06s_imp","f07o_imp","f98bg1_ind","f98bg2_ind","f98bg3_ind","f98bg4_ind","f98bg5_ind","f99g1_ind", "f99g2_ind", "f99g3_ind", "f99g4_ind","fg4s1_ind","f52g3_ind", "f99fg2_ind","year")
labels=c("respid","Gender","birthyr","Health","Occupational","Financial","Recreational","Driving","f99g1_imp", "f99g2_imp", "f99g3_imp", "f99g4_imp","fg4s1_imp","f52g3_imp", "f99fg2_imp","year")


# 2001 (contains risk propensity in 5 domains)
data <- read.spss("2001/ZA4051_1.sav", to.data.frame = T,use.value.labels = F)
data <- data[,vars_imp[c(1:8,14:16)]] 
indicators <- read.spss("2001/ZA4051_indicator.sav",to.data.frame = T,use.value.labels = F)
data2001 <- removeimp(data,indicators,vars_imp[c(4:8,14:15)],vars_ind[c(4:8,14:15)],labels[c(1:8,14:16)])  
data2001 <- data2001 %>%
  select("respid","Gender","birthyr","Health","Occupational","Financial","Recreational","Driving","year","age") %>%
  mutate(SYEAR=2001)

# 2003-04 (contains risk propensity in 5 domains)
data <- read.spss("2003-04/ZA4436_1.sav", to.data.frame = T,use.value.labels = F)
data <- data[,vars_imp[c(1:12,14,16)]] 
indicators=read.spss("2003-04/ZA4436_indicator.sav",to.data.frame = T,use.value.labels = F)
data2003=removeimp(data,indicators,vars_imp[c(4:12,14)],vars_ind[c(4:12,14)],labels[c(1:12,14,16)])
data2003 <- data2003 %>%
  select("respid","Gender","birthyr","Health","Occupational","Financial","Recreational","Driving","year","age") %>%
  mutate(SYEAR=2003)

# 2005
data=read.spss("2005/ZA4437_1_v5-0-0.sav",to.data.frame = T,use.value.labels = F)
data=data[,vars_imp[c(1:14,16)]] 
indicators=read.spss("2005/ZA4437_indicator_v5-0-0.sav",to.data.frame = T,use.value.labels = F)
data2005=removeimp(data,indicators,vars_imp[c(4:14)],vars_ind[c(4:14)],labels[c(1:14,16)])
data2005 <- data2005 %>%
  select("respid","Gender","birthyr","Health","Occupational","Financial","Recreational","Driving","year","age") %>%
  mutate(SYEAR=2005)

# 2006
data=read.spss("2006/ZA4521_1.sav",to.data.frame = T,use.value.labels = F)
data=data[,vars_imp[c(1:14,16)]] 
indicators=read.spss("2006/ZA4521_indicator.sav",to.data.frame = T,use.value.labels = F)
data2006=removeimp(data,indicators,vars_imp[c(4:14)],vars_ind[c(4:14)],labels[c(1:14,16)])
data2006 <- data2006 %>%
  select("respid","Gender","birthyr","Health","Occupational","Financial","Recreational","Driving","year","age") %>%
  mutate(SYEAR=2006)

# 2007
data=read.spss("2007/ZA4740_1.sav",to.data.frame = T,use.value.labels = F)
data=data[,vars_imp[c(1:8,13:14,16)]]
indicators=read.spss("2007/ZA4740_indicator.sav",to.data.frame = T,use.value.labels = F)
data2007=removeimp(data,indicators,vars_imp[c(4:8,13:14)],vars_ind[c(4:8,13:14)],labels[c(1:8,13:14,16)])
data2007 <- data2007 %>%
  select("respid","Gender","birthyr","Health","Occupational","Financial","Recreational","Driving","year","age") %>%
  mutate(SYEAR=2007)


# 2008
data=read.spss("2008/ZA4970_1.sav",to.data.frame = T,use.value.labels = F)
data=data[,vars_imp[c(1:8,13:14,16)]] 
indicators=read.spss("2008/ZA4970_indicator.sav",to.data.frame = T,use.value.labels = F)
data2008=removeimp(data,indicators,vars_imp[c(4:8,13:14)],vars_ind[c(4:8,13:14)],labels[c(1:8,13:14,16)])
data2008 <- data2008 %>%
  select("respid","Gender","birthyr","Health","Occupational","Financial","Recreational","Driving","year","age") %>%
  mutate(SYEAR=2008)

# 2009 
data=read.spss("2009/ZA5230_1.sav",to.data.frame = T,use.value.labels = F)
data=data[,vars_imp[c(1:8,16)]] 
indicators=read.spss("2009/ZA5230_indicator.sav",to.data.frame = T,use.value.labels = F)
data2009=removeimp(data,indicators,vars_imp[c(4:8)],vars_ind[c(4:8)],labels[c(1:8,16)])
data2009 <- data2009 %>%
  select("respid","Gender","birthyr","Health","Occupational","Financial","Recreational","Driving","year","age") %>%
  mutate(SYEAR=2009)

# 2010
data=read.spss("2010/ZA5292_1_v1-0-0.sav",to.data.frame = T,use.value.labels = F)
data=data[,vars_imp[c(1:8,16)]] 
indicators=read.spss("2010/ZA5292_indicator_v1-0-0.sav",to.data.frame = T,use.value.labels = F)
data2010=removeimp(data,indicators,vars_imp[c(4:8)],vars_ind[c(4:8)],labels[c(1:8,16)])
data2010 <- data2010 %>%
  select("respid","Gender","birthyr","Health","Occupational","Financial","Recreational","Driving","year","age") %>%
  mutate(SYEAR=2010)

# 2011-12 (doesn't have risk items)

# 2013
data=read.spss("2013/ZA5647_1_v1-0-0.sav",to.data.frame = T,use.value.labels = F)
data=data[,vars_imp[c(1:8,16)]] 
indicators=read.spss("2013/ZA5647_indicator_v1-0-0.sav",to.data.frame = T,use.value.labels = F)
data2013=removeimp(data,indicators,vars_imp[c(4:8)],vars_ind[c(4:8)],labels[c(1:8,16)])
data2013 <- data2013 %>%
  select("respid","Gender","birthyr","Health","Occupational","Financial","Recreational","Driving","year","age")%>%
  mutate(SYEAR=2013)

### combine all waves data
### preprocess and clean data
SAVE_preprocess <- rbind(data2001, data2003, data2005, data2006, data2007, data2008, data2009,data2010, data2013) %>%
  mutate(BIRTH=year-age) %>%
  dplyr::rename(ID=respid) %>%
  select(ID, SYEAR, Gender, age, Driving, Financial, Recreational, Occupational, Health) %>%
  dplyr::filter(!is.na(Gender)) %>%
  dplyr::filter(!is.na(age)) %>%
  dplyr::filter(age >= 18 & age <= 90) %>%
  filter(!(is.na(Driving) & is.na(Financial) & is.na(Recreational) & is.na(Occupational) & is.na(Health)))

### check Gender and delete participants have different Gender
SAVE_ID_Gender <- SAVE_preprocess %>%
  dplyr::select(ID, Gender)

unique_SAVE_ID_Gender <-  unique(SAVE_ID_Gender) # get unique rows: unique function can eliminate duplicate rows
dupIDs <- with(unique_SAVE_ID_Gender, ID[duplicated(ID)])  # duplicate IDs, which means diffferent Gender: duplicated function can be used to identify duplicate ids after duplicate rows are removed
errorSAVE<- subset(SAVE_preprocess, ID %in% dupIDs)  
rightSAVE<- subset(SAVE_preprocess, !(ID %in% dupIDs)) 

### just keep participants >=2 records
dup_SAVE<- rightSAVE%>%
  dplyr::select(ID, age, Gender, SYEAR) %>%
  filter(!is.na(Gender)) %>%
  group_by(ID) %>%
  filter(n()>=2) %>%
  ungroup()

dup_SAVE_ID <- unique(dup_SAVE$ID)
SAVE_process1 <- subset(rightSAVE, !(ID %in% dup_SAVE_ID)) 
SAVE_process2 <- subset(rightSAVE, (ID %in% dup_SAVE_ID)) 

### cleaned data
SAVE_process <- SAVE_process2

### save 
setwd("~/Desktop/analysis/meta_analysis_risk_normalized_5/data/process_data")
saveRDS(SAVE_process, file = "SAVE_process.Rds")



### set age group
agebins = c(18, seq(25, 90, 5))
SAVE_process$age_bins <- cut(SAVE_process$age, breaks=agebins, include.lowest = TRUE, right = TRUE, lable = TRUE)

SAVE_process_reference <- SAVE_process %>%
  filter(age_bins=="(50,55]")

mean_Driving <- mean(SAVE_process_reference$Driving, na.rm = TRUE)
sd_Driving <- sd(SAVE_process_reference$Driving, na.rm = TRUE)

mean_Financial <- mean(SAVE_process_reference$Financial, na.rm = TRUE)
sd_Financial <- sd(SAVE_process_reference$Financial, na.rm = TRUE)

mean_Recreational <- mean(SAVE_process_reference$Recreational, na.rm = TRUE)
sd_Recreational <- sd(SAVE_process_reference$Recreational, na.rm = TRUE)

mean_Occupational <- mean(SAVE_process_reference$Occupational, na.rm = TRUE)
sd_Occupational <- sd(SAVE_process_reference$Occupational, na.rm = TRUE)

mean_Health <- mean(SAVE_process_reference$Health, na.rm = TRUE)
sd_Health <- sd(SAVE_process_reference$Health, na.rm = TRUE)

SAVE_process_z <- SAVE_process %>%
  mutate(Driving_z = (Driving - mean_Driving)/sd_Driving,
         Financial_z = (Financial - mean_Financial)/sd_Financial,
         Recreational_z = (Recreational - mean_Recreational)/sd_Recreational,
         Occupational_z = (Occupational - mean_Occupational)/sd_Occupational,
         Health_z = (Health - mean_Health)/sd_Health)


#save data
setwd("~/Desktop/analysis/meta_analysis_risk_normalized_5/data/process_z_data")
saveRDS(SAVE_process_z, file = "SAVE_process_z.Rds")

#SAVE_process_z <- readRDS(file = "SAVE_process_z.Rds")

