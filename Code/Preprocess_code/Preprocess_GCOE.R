###GCOE Preprocess###
rm(list=ls())
# Japan_2004_q27;
# Japan_2005_q22;
# Japan_2006_q19;
# Japan_2007_q22;
# Japan_2008_q26;
# Japan_2009_q27;
# Japan_2010_q23;

# USA_2005_q22;
# USA_2006_q19;
# USA_2007_q22;
# USA_2008_q26;
# USA_2009_q27;
# USA_2010_q23;

# China/India just survey 2 years

library(tidyverse)
library(haven)

setwd("../rawdata/GCOE_data")

###Japan GCOE Preprocess
###2004-2010###

# Japan_2004_q27;
syear = 2004
Japan2004 <- read_dta("Japan/2004Data_JAPAN_prefid.dta") %>%
  select(seqno, panelid, q29, fq1, fq4) # q29:00…10,NA=99;  fq1:1…2,NA=9;  fq4:1…7,NA=9
Japan2004$SYEAR = syear
Japan2004$ID = paste(Japan2004$seqno, Japan2004$panelid, sep="_")
Japan2004 <- Japan2004 %>%
  rename(General=q29, Gender=fq1, age_cat=fq4) %>%
  mutate(age=NA)

# Japan_2005_q22;
syear = 2005
Japan2005 <- read_dta("Japan/2005Data_JAPAN_prefid.dta") %>%
  select(seqno, panelid, sex, q22, fq3) %>% #sex:1=male, 2=female;  fq22:00…10,NA=99; fq3:birthyear? NA=99
  mutate(fq3 = fq3+1925) # For the Japan 2005 data, the variables in fq3 are the Japanese calendar year. Please add 1925 to the variable in fq3 to convert it to the Western calendar year (AD).
Japan2005$SYEAR = syear
Japan2005$ID = paste(Japan2005$seqno, Japan2005$panelid, sep="_")
Japan2005 <- Japan2005 %>%
  rename(General=q22, Gender=sex, BIRTH=fq3) %>%
  mutate(age=SYEAR-BIRTH)  %>% #real age
  filter((!is.na(BIRTH)))   # no BIRTH=NA

# Japan_2006_q19;
syear = 2006
Japan2006 <- read_dta("Japan/2006Data_JAPAN_prefid.dta") %>%
  select(seqno, panelid, q19, fq1, fq3) #fq19:00…10,NA=99; fq1:1-2; fq3:birthyear
Japan2006$SYEAR = syear
Japan2006$ID = paste(Japan2006$seqno, Japan2006$panelid, sep="_")
Japan2006 <- Japan2006 %>%
  rename(General=q19, Gender=fq1, BIRTH=fq3) %>%
  mutate(age=SYEAR-BIRTH) %>% #real age
  filter((!is.na(BIRTH)))   # no BIRTH=NA

# Japan_2007_q22;
syear = 2007
Japan2007 <- read_dta("Japan/2007Data_JAPAN_prefid.dta") %>%
  select(seqno, panelid, q22, fq1, fq6)  #fq22:00…10,NA=99; fq1:1-2; fq6:birthyear
Japan2007$SYEAR = syear
Japan2007$ID = paste(Japan2007$seqno, Japan2007$panelid, sep="_")
Japan2007 <- Japan2007 %>%
  rename(General=q22, Gender=fq1, BIRTH=fq6) %>%
  mutate(age=SYEAR-BIRTH) %>% #real age
  filter((!is.na(BIRTH)))   # no BIRTH=NA

# Japan_2008_q26;
syear = 2008
Japan2008 <- read_dta("Japan/2008Data_JAPAN_prefid.dta") %>%
  select(seqno, panelid, q26, fq1, fq6) #fq26:00…10,NA=99; fq1:1-2; fq6:birthyear
Japan2008$SYEAR = syear  
Japan2008$ID = paste(Japan2008$seqno, Japan2008$panelid, sep="_")
Japan2008 <- Japan2008 %>%
  rename(General=q26, Gender=fq1, BIRTH=fq6) %>%
  mutate(age=SYEAR-BIRTH) %>% #real age
  filter((!is.na(BIRTH)))   # no BIRTH=NA

# Japan_2009_q27;
syear = 2009
Japan2009 <- read_dta("Japan/2009Data_JAPAN_prefid.dta") %>%
  select(seqno, panelid, q27, fq1, fq6) #fq27:00…10,NA=99; fq1:1-2; fq6:birthyear
Japan2009$SYEAR <- syear  
Japan2009$ID = paste(Japan2009$seqno, Japan2009$panelid, sep="_")
Japan2009 <- Japan2009 %>%
  rename(General=q27, Gender=fq1, BIRTH=fq6) %>%
  mutate(age=SYEAR-BIRTH) %>% #real age
  filter((!is.na(BIRTH)))   # no BIRTH=NA

# Japan_2010_q23;
syear = 2010
Japan2010 <- read_dta("Japan/2010Data_JAPAN_prefid.dta") %>%
  select(seqno, panelid, q23, fq1, fq4) #fq23:00…10,NA=99; fq1:1-2; fq4:birthyear
Japan2010$SYEAR = syear  
Japan2010$ID = paste(Japan2010$seqno, Japan2010$panelid, sep="_")
Japan2010 <- Japan2010 %>%
  rename(General=q23, Gender=fq1, BIRTH=fq4) %>%
  mutate(age=SYEAR-BIRTH) %>% #real age
  filter((!is.na(BIRTH)))   # no BIRTH=NA

### combine all waves data
Japan_process_long <- Reduce(function(x, y) merge(x, y, all=TRUE), list(Japan2004, Japan2005, Japan2006, Japan2007, Japan2008, Japan2009, Japan2010)) %>%
  select(ID, Gender, SYEAR, BIRTH, age, General) 

### reshape
Japan_process_wide <- Japan_process_long  %>%
  select(ID, Gender, SYEAR, BIRTH, age) %>%
  pivot_wider(names_from = "SYEAR", values_from = "age")  %>%  #pivot_wider：from long to wide;
  filter((!is.na(BIRTH) & !is.na("2004"))) %>%
  mutate("2004" = 2004-BIRTH, "2005"= 2005-BIRTH, "2006"= 2006-BIRTH, "2007"= 2007-BIRTH, "2008"= 2008-BIRTH, "2009"= 2009-BIRTH, "2010"= 2010-BIRTH)

age_process_long <- Japan_process_wide %>%
  pivot_longer(4:10, names_to = "SYEAR", values_to = "age") #pivot_longer:from wide to long

Japan_process_long <- Japan_process_long %>%
  select(ID, Gender, SYEAR,  General)

### preprocess and clean data
GCOE_Japan_preprocess <- merge(age_process_long, Japan_process_long, by = c("ID", "Gender", "SYEAR"), all = TRUE) %>%
  select(ID, Gender, SYEAR, BIRTH,age, General) %>%
  filter(!is.na(Gender)) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(General)) %>%
  filter(General !=99) %>%
  filter(age >= 18 & age <= 90) 

### check Gender and delete participants have different Gender
GCOE_Japan_ID_Gender <- GCOE_Japan_preprocess %>%
  select(ID, Gender)

unique_GCOE_Japan_ID_Gender =  unique(GCOE_Japan_ID_Gender) # get unique rows: unique function can eliminate duplicate rows
dupIDs = with(unique_GCOE_Japan_ID_Gender, ID[duplicated(ID)])  # duplicate IDs, which means diffferent Gender: duplicated function can be used to identify duplicate ids after duplicate rows are removed
errorGCOE_Japan = subset(GCOE_Japan_preprocess, ID %in% dupIDs)  
rightGCOE_Japan = subset(GCOE_Japan_preprocess, !(ID %in% dupIDs)) 

### just keep participants >=2 records
dup_GCOE_Japan <- rightGCOE_Japan %>%
  select(ID, age, Gender, SYEAR) %>%
  group_by(ID) %>%
  filter(n()>=2) %>%
  ungroup()

dup_GCOE_Japan_ID = unique(dup_GCOE_Japan$ID)
GCOE_Japan_process1 = subset(rightGCOE_Japan, !(ID %in% dup_GCOE_Japan_ID)) 
GCOE_Japan_process2 = subset(rightGCOE_Japan, (ID %in% dup_GCOE_Japan_ID)) 

### cleaned data
GCOE_Japan_process = GCOE_Japan_process2

### Save object in Rds format
setwd("../data/process_data")
saveRDS(GCOE_Japan_process, file = "GCOE_Japan_process.Rds")


### set age group
agebins = c(18, seq(25,90,5))
GCOE_Japan_process$age_bins = cut(GCOE_Japan_process$age, breaks=agebins, include.lowest = TRUE, right = TRUE, lable = TRUE)

GCOE_Japan_process_reference <- GCOE_Japan_process %>%
  filter(age_bins == "(50,55]")

mean_General = mean(GCOE_Japan_process_reference$General, na.rm = TRUE)
sd_General = sd(GCOE_Japan_process_reference$General, na.rm = TRUE)

GCOE_Japan_process_z <- GCOE_Japan_process %>%
  mutate(General_z = (General - mean_General)/sd_General)

### Save object in Rds format
setwd("../data/process_z_data")
saveRDS(GCOE_Japan_process_z, file = "GCOE_Japan_process_z.Rds")




###USA GCOE Preprocess###
rm(list=ls())
###2005-2010###

setwd("../data/rawdata/GCOE_data")

# USA_2005_q22;
syear = 2005 
USA2005 <- read_dta("USA/2005_USA.dta") %>%
  select(PANEL_ID, SEX, AGE, q22, q57you, Unintended_2005) %>% 
  rename(ID=PANEL_ID,  Gender=SEX, General=q22, BIRTH=q57you, Unintended_year=Unintended_2005) %>% 
  mutate(age=2005-BIRTH)
USA2005$SYEAR = syear  

# USA_2006_q19;
syear = 2006
USA2006 <- read_dta("USA/2006_USA.dta") %>%
  select(PANEL_ID, SEX, AGE, q19, q50_you_year, Unintended_2006) %>% 
  rename(ID=PANEL_ID, Gender=SEX, General=q19, BIRTH=q50_you_year, Unintended_year=Unintended_2006) %>% 
  mutate(age=2006-BIRTH)
USA2006$SYEAR = syear 

# USA_2007_q22;
syear = 2007
USA2007 <- read_dta("USA/2007_USA.dta") %>%
  select(PANEL_ID, SEX, AGE, q22, bq6_you_year, Unintended_2007) %>% 
  rename(ID=PANEL_ID, Gender=SEX, General=q22, BIRTH=bq6_you_year, Unintended_year=Unintended_2007) %>%
  mutate(age=2007-BIRTH)
USA2007$SYEAR = syear 

# USA_2008_q26;
syear = 2008
USA2008 <- read_dta("USA/2008_USA.dta") %>%
  select(PANEL_ID, SEX, AGE, q26, bq6_you_year, Unintended_2008) %>% 
  rename(ID=PANEL_ID, Gender=SEX, General=q26, BIRTH=bq6_you_year, Unintended_year=Unintended_2008)  %>%
  mutate(age=2008-BIRTH)
USA2008$SYEAR = syear 

# USA_2009_q27;
syear = 2009
USA2009 <- read_dta("USA/2009_USA.dta") %>%
  select(PANEL_ID, SEX, AGE, q27, bq6_you_year, Unintended_2009) %>% 
  rename(ID=PANEL_ID, Gender=SEX, General=q27, BIRTH=bq6_you_year, Unintended_year=Unintended_2009) %>%
  mutate(age=2009-BIRTH)
USA2009$SYEAR = syear 

# USA_2010_q23;
syear = 2010
USA2010 <- read_dta("USA/2010_USA.dta") %>%
  select(PANEL_ID, SEX, AGE, q23, bq4_you_year, Unintended_2010) %>% 
  rename(ID=PANEL_ID, Gender=SEX, General=q23, BIRTH=bq4_you_year, Unintended_year=Unintended_2010) %>% 
  mutate(age=2010-BIRTH)
USA2010$SYEAR = syear 

### combine all waves data
USA_process_long <- Reduce(function(x, y) merge(x, y, all=TRUE), list(USA2005, USA2006, USA2007, USA2008, USA2009,USA2010)) %>%
  select(ID, Gender, AGE, SYEAR, BIRTH,  age, General, Unintended_year) %>%  # we used age calculated by SYEAR-BIRTH rathere than the raw AGE
  filter(is.na(Unintended_year) | Unintended_year!=1)  # remove Unintended_year==1, Do not use the observations with the variable “Unintended_[year]==1” for any panel analysis(readme.pdf)

### preprocess and clean data
GCOE_USA_preprocess <- USA_process_long %>%
  select(ID, Gender, SYEAR, BIRTH, age, General) %>%
  filter(!is.na(Gender)) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(General))  %>%
  dplyr::filter(age >= 18 & age <= 90) 

### check Gender and delete participants have different Gender
GCOE_USA_ID_Gender <- GCOE_USA_preprocess %>%
  select(ID, Gender)

unique_GCOE_USA_ID_Gender =  unique(GCOE_USA_ID_Gender) # get unique rows: unique function can eliminate duplicate rows
dupIDs = with(unique_GCOE_USA_ID_Gender, ID[duplicated(ID)])  # duplicate IDs, which means diffferent Gender: duplicated function can be used to identify duplicate ids after duplicate rows are removed
errorGCOE_USA = subset(GCOE_USA_preprocess, ID %in% dupIDs)  
rightGCOE_USA = subset(GCOE_USA_preprocess, !(ID %in% dupIDs)) 

### just keep participants >=2 records
dup_GCOE_USA <- rightGCOE_USA %>%
  select(ID, age, Gender, SYEAR) %>%
  group_by(ID) %>%
  filter(n()>=2) %>%
  ungroup()

dup_GCOE_USA_ID = unique(dup_GCOE_USA$ID)   # Participant ID have duplicate records
GCOE_USA_process1 = subset(rightGCOE_USA, !(ID %in% dup_GCOE_USA_ID)) # Participant have <2 records
GCOE_USA_process2 = subset(rightGCOE_USA, (ID %in% dup_GCOE_USA_ID))  # Participant have >=2 records

### cleaned data
GCOE_USA_process = GCOE_USA_process2


### Save object in Rds format
setwd("../data/process_data")
saveRDS(GCOE_USA_process, file = "GCOE_USA_process.Rds")


### set age group
agebins = c(18, seq(25, 90, 5))
GCOE_USA_process$age_bins = cut(GCOE_USA_process$age, breaks=agebins, include.lowest = TRUE, right = TRUE, lable = TRUE)

GCOE_USA_process_reference <- GCOE_USA_process %>%
  filter(age_bins == "(50,55]")

mean_General = mean(GCOE_USA_process_reference$General, na.rm = TRUE)
sd_General = sd(GCOE_USA_process_reference$General, na.rm = TRUE)

GCOE_USA_process_z <- GCOE_USA_process %>%
  mutate(General_z = (General - mean_General)/sd_General)

### Save object in Rds format
setwd("../data/process_z_data")
saveRDS(GCOE_USA_process_z, file = "GCOE_USA_process_z.Rds")

