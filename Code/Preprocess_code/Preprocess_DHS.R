###DHS Prprocess###
rm(list=ls())

#variables
#nohhold: Household index
#nomem: Index of member of the household
#gebjaar: Year of birth of the respondent
#geslacht: Sex of the respondent
#1.spaar1: reverse
#2.spaar2: reverse
#3.spaar3: 
#4.spaar4: reverse
#5.spaar5: 
#6.spaar6: 

#install.packages("tidyverse")
#install.packages("foreign")
library(foreign)
library(tidyverse)

years = 1993:2020

path = "../rawdata/DHS_data"
setwd(path)

### risk-taing data
filespsy = dir(path, pattern = "psy", full.names = TRUE, ignore.case = TRUE) # risk item is always in psy SPSS files

for(i in 1:length(years)){
  syear = years[i]
  obj_name = paste0("psy",years[i])
  file = filespsy[grep(pattern=obj_name, x = filespsy)]
  input <- read.spss(file, to.data.frame=T, use.value.labels=F, reencode=F)
  input <- input %>%  
    select(nohhold, nomem,
           spaar1, spaar2, spaar3, spaar4, spaar5, spaar6)
  input$SYEAR = syear
  input$ID = paste(input$nohhold, input$nomem, sep="_")
  assign(obj_name, value = input)
}

psy_total <- rbind(psy1993, psy1994, psy1995, psy1996, psy1997, psy1998, psy1999, 
                   psy2000, psy2001, psy2002, psy2003, psy2004, psy2005, psy2006, psy2007, psy2008, psy2009, 
                   psy2010, psy2011, psy2012, psy2013, psy2014, psy2015, psy2016, psy2017, psy2018, psy2019,
                   psy2020)


### demographics data
filesdems = matrix(data=NA,nrow=28,ncol=1)
filesdems[1] = dir(path,pattern = "wrk1993", full.names = TRUE, ignore.case = TRUE) # demographics are given in wrk SPSS in 1993
filesdems[2:28] = dir(path,pattern = "hh", full.names = TRUE, ignore.case = TRUE) # demographics are given in hhi SPSS from 1994 onwards

for(i in 1:length(filesdems)){
  syear = years[i]
  obj_name = paste0("dems", years[i])
  input <- read.spss(filesdems[i], to.data.frame = T,use.value.labels = F, reencode=F)
  input <- input %>%  
    select(nohhold, nomem,
           gebjaar, geslacht)
  input$ID = paste(input$nohhold, input$nomem, sep="_")
  assign(obj_name, value = input)
}

dems_total <- rbind(dems1993, dems1994, dems1995, dems1996, dems1997, dems1998, dems1999, 
                    dems2000, dems2001, dems2002, dems2003, dems2004, dems2005, dems2006, dems2007, dems2008, dems2009, 
                    dems2010, dems2011, dems2012, dems2013, dems2014, dems2015, dems2016, dems2017, dems2018, dems2019,
                    dems2020)


### combine all waves data
DHS_data <- merge(dems_total, psy_total, by = c("nohhold", "nomem", "ID"), all = TRUE) %>%
  unique() 


### recode variables to be on a 0-10 scale
DHS_data <- DHS_data %>%
  mutate(spaar1_reverse = 8-spaar1, spaar2_reverse = 8-spaar2, spaar4_reverse = 8-spaar4) %>%
  mutate(Financial_raw = (spaar1_reverse + spaar2_reverse + spaar3 + spaar4_reverse + spaar5 + spaar6)/6) %>%
  mutate(spaar1_new = 10*(spaar1_reverse-1)/(7-1), spaar2_new = 10*(spaar2_reverse-1)/(7-1), spaar3_new = 10*(spaar3-1)/(7-1), spaar4_new = 10*(spaar4_reverse-1)/(7-1), spaar5_new = 10*(spaar5-1)/(7-1), spaar6_new = 10*(spaar6-1)/(7-1)) %>% 
  mutate(Financial = (spaar1_new + spaar2_new + spaar3_new + spaar4_new + spaar5_new + spaar6_new)/6) 


### preprocess and clean data
DHS_preprocess <- DHS_data %>%
  rename(BIRTH=gebjaar, Gender=geslacht) %>%
  select(ID, BIRTH, Gender, SYEAR, Financial_raw, Financial) %>%
  filter(BIRTH > 0) %>%
  filter(SYEAR > 0) %>%
  filter(!is.na(Gender)) %>%
  mutate(age = SYEAR-BIRTH) %>%
  select(ID, SYEAR, Gender, age, Financial_raw, Financial) %>%
  filter(!is.na (Financial)) %>%
  filter(!is.na(age)) %>%
  filter(age >= 18 & age <= 90) 

### check Gender and delete participants have different Gender
DHS_ID_Gender <- DHS_preprocess %>%
  select(ID, Gender)

unique_DHS_ID_Gender =  unique(DHS_ID_Gender) # get unique rows: unique function can eliminate duplicate rows
dupIDs = with(unique_DHS_ID_Gender, ID[duplicated(ID)])  # duplicate IDs, which means diffferent Gender: duplicated function can be used to identify duplicate ids after duplicate rows are removed
errorDHS = subset(DHS_preprocess, ID %in% dupIDs)  
rightDHS = subset(DHS_preprocess, !(ID %in% dupIDs)) 

### just keep participants >=2 records
dup_DHS <- rightDHS %>%
  select(ID, age, Gender, SYEAR) %>%
  filter(!is.na(Gender)) %>%
  group_by(ID) %>%
  filter(n()>=2) %>%
  ungroup()

dup_DHS_ID = unique(dup_DHS$ID)   # Participant ID have duplicate records
DHS_process1 = subset(rightDHS, !(ID %in% dup_DHS_ID)) # Participant have <2 records
DHS_process2 = subset(rightDHS, (ID %in% dup_DHS_ID))  # Participant have >=2 records

### cleaned data
DHS_process = DHS_process2

### save object in Rds format
setwd("../data/process_data")
saveRDS(DHS_process, file = "DHS_process.Rds")


  
### set age group
agebins = c(18, seq(25, 90, 5))
DHS_process$age_bins = cut(DHS_process$age, breaks=agebins, include.lowest=TRUE, right=TRUE, lable=TRUE)

DHS_process_reference <- DHS_process %>%
  filter(age_bins == "(50,55]")

mean_Financial = mean(DHS_process_reference$Financial_raw, na.rm = TRUE)
sd_Financial = sd(DHS_process_reference$Financial_raw, na.rm = TRUE)

DHS_process_z <- DHS_process %>%
  mutate(Financial_z = (Financial_raw - mean_Financial)/sd_Financial)

### Save object in Rds format
setwd("../data/process_z_data")
saveRDS(DHS_process_z, file = "DHS_process_z.Rds")





  