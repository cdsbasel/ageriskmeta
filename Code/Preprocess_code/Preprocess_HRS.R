###HRS Prprocess###
rm(list=ls())

library(foreign)
library(tidyverse)
library(SAScii)

setwd("../rawdata/HRS_data")

###2014
###Section PR: Preload (Respondent) --- H14PR_R
#HHID: HOUSEHOLD IDENTIFICATION NUMBER
#PN: RESPONDENT PERSON IDENTIFICATION NUMBER    ###id=paste(dems$HHID,dems$PN,sep="")
#OX060_R: Gender OF INDIVIDUAL-UPDATED   1.  MALE  2.  FEMALE
#OX004_R: MONTH BORN-UPDATED
#OX067_R: YEAR BORN-UPDATED

###Section A: Coverscreen (Respondent) --- H14A_R
#OA500: DATE OF INTERVIEW - MONTH
#OA501: DATE OF INTERVIEW - YEAR

###Section B: Demographics (Respondent) --- H14B_R
#OB132:WHAT PER CENT TAKE RISKS?  0(not at all willing to take risks)-10(very willing to take risks) scale

###Section LB: Leave-Behind Questionnaires (Respondent) --- H14LB_R
#OLB032_1: RISKS WHILE DRIVING
#OLB032_2: RISKS IN FINANCIAL MATTERS
#OLB032_3: RISKS DURING LEISURE AND SPORT
#OLB032_4: RISKS IN YOUR OCCUPATION
#OLB032_5: RISKS WITH YOUR HEALTH


###2014
path_da = "../rawdata/HRS_data/2014_core/h14da/"
path_sas = "../rawdata/HRS_data/2014_core/h14sas/"

file = "H14PR_R"
H14PR_R <- read.SAScii(paste(path_da, file, ".da", sep=""), paste(path_sas, file, ".sas", sep="")) %>%
  select(HHID, PN, OX060_R, OX067_R, OX004_R) %>%
  mutate(ID=paste(HHID, PN, sep="")) %>%
  rename(Gender=OX060_R, birth_year=OX067_R, birth_mo=OX004_R) %>%
  mutate(birthdate = as.Date(paste(birth_year, birth_mo, "01", sep="-"), "%Y-%m-%d")) 
  
file = "H14A_R"
H14A_R <- read.SAScii(paste(path_da, file, ".da", sep=""), paste(path_sas, file, ".sas", sep="")) %>%
  select(HHID, PN, OA501, OA500) %>%
  mutate(ID=paste(HHID, PN, sep="")) %>%
  rename(int_year=OA501, int_mo=OA500) %>%
  mutate(intdate = as.Date(paste(int_year, int_mo,"01",sep="-"), "%Y-%m-%d")) 

file = "H14B_R"
H14B_R <- read.SAScii(paste(path_da, file, ".da", sep=""), paste(path_sas, file, ".sas", sep="")) %>%
  select(HHID, PN, OB132) %>%
  mutate(ID=paste(HHID, PN, sep="")) %>%
  rename(General=OB132) %>%
  filter(General<=10)

file="H14LB_R"
H14LB_R <- read.SAScii(paste(path_da, file, ".da", sep=""), paste(path_sas, file, ".sas", sep="")) %>%
  select(HHID, PN, OLB032_1, OLB032_2, OLB032_3, OLB032_4, OLB032_5) %>%
  mutate(ID=paste(HHID, PN, sep="")) %>%
  rename(Driving=OLB032_1, Financial=OLB032_2, Recreational=OLB032_3, Occupational=OLB032_4, Health=OLB032_5)
  
HRS_2014 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(H14PR_R, H14A_R, H14B_R, H14LB_R)) %>%
  mutate(SYEAR=2014)


###2016
###Section PR: Preload (Respondent) --- H16PR_R
#HHID: HOUSEHOLD IDENTIFICATION NUMBER
#PN: RESPONDENT PERSON IDENTIFICATION NUMBER    ###id=paste(dems$HHID,dems$PN,sep="")
#PX060_R: Gender OF INDIVIDUAL-UPDATED   1.  MALE  2.  FEMALE
#PX004_R: MONTH BORN-UPDATED
#PX067_R: YEAR BORN-UPDATED

###Section A: Coverscreen (Respondent) --- H16A_R
#PA500: DATE OF INTERVIEW - MONTH
#PA501: DATE OF INTERVIEW - YEAR

###Section B: Demographics (Respondent) --- H16B_R
#PB132:WHAT PER CENT TAKE RISKS?  0(not at all willing to take risks)-10(very willing to take risks) scale

###Section LB: Leave-Behind Questionnaires (Respondent) --- H16LB_R
#PLB032_1: RISKS WHILE DRIVING
#PLB032_2: RISKS IN FINANCIAL MATTERS
#PLB032_3: RISKS DURING LEISURE AND SPORT
#PLB032_4: RISKS IN YOUR OCCUPATION
#PLB032_5: RISKS WITH YOUR HEALTH


path_da = "../rawdata/HRS_data/2016_core/h16da/"
path_sas = "../rawdata/HRS_data/2016_core/h16sas/"

file = "H16PR_R"
H16PR_R <- read.SAScii(paste(path_da, file, ".da", sep=""), paste(path_sas, file, ".sas", sep="")) %>%
  select(HHID, PN, PX060_R, PX067_R, PX004_R) %>%
  mutate(ID=paste(HHID, PN, sep="")) %>%
  rename(Gender=PX060_R, birth_year=PX067_R, birth_mo=PX004_R) %>%
  mutate(birthdate=as.Date(paste(birth_year, birth_mo,"01",sep="-"), "%Y-%m-%d")) 

file = "H16A_R"
H16A_R <- read.SAScii(paste(path_da, file, ".da", sep=""), paste(path_sas, file, ".sas", sep="")) %>%
  select(HHID, PN, PA501, PA500) %>%
  mutate(ID=paste(HHID, PN, sep="")) %>%
  rename(int_year=PA501, int_mo=PA500) %>%
  mutate(intdate=as.Date(paste(int_year, int_mo,"01",sep="-"), "%Y-%m-%d")) 

file = "H16B_R"
H16B_R <- read.SAScii(paste(path_da, file, ".da", sep=""), paste(path_sas, file, ".sas", sep="")) %>%
  select(HHID, PN, PB132) %>%
  mutate(ID=paste(HHID, PN, sep="")) %>%
  rename(General=PB132) %>%
  filter(General<=10)

file = "H16LB_R"
H16LB_R <- read.SAScii(paste(path_da, file, ".da", sep=""), paste(path_sas, file, ".sas", sep="")) %>%
  select(HHID, PN, PLB032_1, PLB032_2, PLB032_3, PLB032_4, PLB032_5) %>%
  mutate(ID=paste(HHID, PN, sep="")) %>%
  rename(Driving=PLB032_1, Financial=PLB032_2, Recreational=PLB032_3, Occupational=PLB032_4, Health=PLB032_5)

HRS_2016 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(H16PR_R, H16A_R, H16B_R, H16LB_R)) %>%
  mutate(SYEAR=2016)

###2018
###Section PR: Preload (Respondent) --- H18PR_R
#HHID: HOUSEHOLD IDENTIFICATION NUMBER
#PN: RESPONDENT PERSON IDENTIFICATION NUMBER    ###id=paste(dems$HHID,dems$PN,sep="")
#QX060_R: Gender OF INDIVIDUAL-UPDATED   1.  MALE  2.  FEMALE
#QX004_R: MONTH BORN-UPDATED
#QX067_R: YEAR BORN-UPDATED

###Section A: Coverscreen (Respondent) --- H18A_R
#QA500: DATE OF INTERVIEW - MONTH
#QA501: DATE OF INTERVIEW - YEAR

###Section B: Demographics (Respondent) --- H18B_R
#QB132: WHAT PER CENT TAKE RISKS?  0(not at all willing to take risks)-10(very willing to take risks) scale

###Section LB: Leave-Behind Questionnaires (Respondent) --- H18LB_R
#QLB032_1: RISKS WHILE DRIVING
#QLB032_2: RISKS IN FINANCIAL MATTERS
#QLB032_3: RISKS DURING LEISURE AND SPORT
#QLB032_4: RISKS IN YOUR OCCUPATION
#QLB032_5: RISKS WITH YOUR HEALTH


path_da = "../rawdata/HRS_data/2018_core/h18da/"
path_sas = "../rawdata/HRS_data/2018_core/h18sas/"

file = "H18PR_R"
H18PR_R <- read.SAScii(paste(path_da, file, ".da", sep=""), paste(path_sas, file, ".sas", sep="")) %>%
  select(HHID, PN, QX060_R, QX067_R, QX004_R) %>%
  mutate(ID = paste(HHID, PN, sep="")) %>%
  rename(Gender=QX060_R, birth_year=QX067_R, birth_mo=QX004_R) %>%
  mutate(birthdate = as.Date(paste(birth_year, birth_mo, "01", sep="-"), "%Y-%m-%d")) 

file = "H18A_R"
H18A_R <- read.SAScii(paste(path_da, file, ".da", sep=""), paste(path_sas, file, ".sas", sep="")) %>%
  select(HHID, PN, QA501, QA500) %>%
  mutate(ID = paste(HHID, PN, sep="")) %>%
  rename(int_year=QA501, int_mo=QA500) %>%
  mutate(intdate = as.Date(paste(int_year, int_mo, "01", sep="-"), "%Y-%m-%d")) 

file = "H18B_R"
H18B_R <- read.SAScii(paste(path_da, file, ".da", sep=""), paste(path_sas, file, ".sas", sep="")) %>%
  select(HHID, PN, QB132) %>%
  mutate(ID = paste(HHID,PN,sep="")) %>%
  rename(General=QB132) %>%
  filter(General<=10)

file = "H18LB_R"
H18LB_R <- read.SAScii(paste(path_da, file, ".da", sep=""), paste(path_sas, file, ".sas", sep="")) %>%
  select(HHID, PN, QLB032_1, QLB032_2, QLB032_3, QLB032_4, QLB032_5) %>%
  mutate(ID=paste(HHID, PN, sep="")) %>%
  rename(Driving=QLB032_1, Financial=QLB032_2, Recreational=QLB032_3, Occupational=QLB032_4, Health=QLB032_5)

HRS_2018 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(H18PR_R, H18A_R, H18B_R, H18LB_R)) %>%
  mutate(SYEAR=2018)


### combine all waves data
### preprocess and clean data
HRS_preprocess <- rbind(HRS_2014, HRS_2016, HRS_2018)  %>%
  mutate(age=as.numeric(round((intdate-birthdate)/365))) %>%
  select(ID, Gender, SYEAR, age, General, Driving, Financial, Recreational, Occupational, Health) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(Gender)) %>%
  filter(age > 50 & age <= 90) %>%
  filter(!(is.na(General) & is.na(Driving) & is.na(Financial) & is.na(Recreational) & is.na(Occupational) & is.na(Health)))

### check Gender and delete participants have different Gender
HRS_ID_Gender <- HRS_preprocess %>%
  select(ID, Gender)

unique_HRS_ID_Gender =  unique(HRS_ID_Gender) # get unique rows: unique function can eliminate duplicate rows
dupIDs = with(unique_HRS_ID_Gender, ID[duplicated(ID)])  # duplicate IDs, which means diffferent Gender: duplicated function can be used to identify duplicate ids after duplicate rows are removed
errorHRS = subset(HRS_preprocess, ID %in% dupIDs)  
rightHRS = subset(HRS_preprocess, !(ID %in% dupIDs)) 

### just keep participants >=2 records
dup_HRS <- rightHRS %>%
  select(ID, age, Gender, SYEAR) %>%
  filter(!is.na(Gender)) %>%
  group_by(ID) %>%
  filter(n()>=2) %>%
  ungroup()

dup_HRS_ID = unique(dup_HRS$ID)
HRS_process1 = subset(rightHRS, !(ID %in% dup_HRS_ID)) 
HRS_process2 = subset(rightHRS, (ID %in% dup_HRS_ID)) 

### cleaned data
HRS_process = HRS_process2


### Save object in Rds format
setwd("../data/process_data")
saveRDS(HRS_process, file = "HRS_process.Rds")


### set age group
agebins = c(18, seq(25, 90, 5))
HRS_process$age_bins = cut(HRS_process$age, breaks=agebins, include.lowest = TRUE, right = TRUE, lable = TRUE)

HRS_process_reference <- HRS_process %>%
  filter(age_bins == "(50,55]")

mean_General = mean(HRS_process_reference$General, na.rm = TRUE)
sd_General = sd(HRS_process_reference$General, na.rm = TRUE)

mean_Driving = mean(HRS_process_reference$Driving, na.rm = TRUE)
sd_Driving = sd(HRS_process_reference$Driving, na.rm = TRUE)

mean_Financial = mean(HRS_process_reference$Financial, na.rm = TRUE)
sd_Financial = sd(HRS_process_reference$Financial, na.rm = TRUE)

mean_Recreational = mean(HRS_process_reference$Recreational, na.rm = TRUE)
sd_Recreational = sd(HRS_process_reference$Recreational, na.rm = TRUE)

mean_Occupational = mean(HRS_process_reference$Occupational, na.rm = TRUE)
sd_Occupational = sd(HRS_process_reference$Occupational, na.rm = TRUE)

mean_Health = mean(HRS_process_reference$Health, na.rm = TRUE)
sd_Health = sd(HRS_process_reference$Health, na.rm = TRUE)

HRS_process_z <- HRS_process %>%
  mutate(General_z = (General - mean_General)/sd_General,
         Driving_z = (Driving - mean_Driving)/sd_Driving,
         Financial_z = (Financial - mean_Financial)/sd_Financial,
         Recreational_z = (Recreational - mean_Recreational)/sd_Recreational,
         Occupational_z = (Occupational - mean_Occupational)/sd_Occupational,
         Health_z = (Health - mean_Health)/sd_Health)
         
### Save object in Rds format
setwd("../data/process_z_data")
saveRDS(HRS_process_z, file = "HRS_process_z.Rds")





  



