###HILDA Preprocess###
rm(list=ls())

# afirisk(W1), bfirisk(W2), cfirisk(W3), dfirisk(W4), ffirisk(W6), hfirisk(W8), jfirisk(W10), kfirisk(W11), lfirisk(W12), mfirisk(W13), nfirisk(W14), ofirisk(W15), pfirisk(W16), qfirisk(W17), rfirisk(W18), sfirisk(W19)
                                                    # ffiriska(W6),hfiriska(W8),jfiriska(W10),kfiriska(W11),lfiriska(W12),mfiriska(W13),nfiriska(W14),ofiriska(W15),pfiriska(W16),qfiriska(W17),rfiriska(W18),sfiriska(W19)

# waves: 1, 2, 3, 4, 6, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19
# waves: a, b, c, d, f, h, j, k, l, m, n , o, p, q, r, s

#wave 1-4 code
 # xwaveid：cross wave Id
 # ahhhqivw: Date of interview?age?   bhhhqivw   chhhqivw  dhhhqivw
 # ahgage1: Date of birth        bhgage1   chgage1   dhgage1
 # ahgsex1:sex                   bhgsex1   chgsex1   dhgsex1
 # afirisk:1-5                   bfirisk   cfirisk   dfirisk

# wave 6-19 code
# xwaveid
# fhhhqivw   hhhhqivw  ...
# fhgage1    hhgage1   ...
# fhgsex1    hhgsex1   ...
# ffirisk    hfirisk   ...
# ffiriska   hfiriska  ...

#install.packages("tidyverse")
#install.packages("foreign")
#install.packages("ggplot2")
library(foreign)
library(tidyverse)
library(ggplot2)
 

waves = c(1:4,6,8,10:19)
waves_letter = c("a", "b", "c", "d", "f", "h", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s")

SYEAR <- c(2001:2004, 2006, 2008, 2010:2019)

path="~/Desktop/analysis/meta_analysis_risk_normalized_5/rawdata/HILDA_data"
setwd(path)
files=dir(path,pattern = "Combined ", full.names = TRUE, ignore.case = TRUE) # risk item is always in psy SPSS files

for(i in 1:length(waves)){
  wavenumber=waves[i]
  waveletter=waves_letter[i]
  wave_name <- paste0("wave", wavenumber)
  obj_name <- paste0("Combined ", waveletter,"190c.sav", sep="")
  file <- files[grep(pattern=obj_name, x = files)]
  input <- read.spss(file, to.data.frame = T, use.value.labels = F, reencode=F)
  
  if(i < 5) {
    input <- input %>%
      dplyr::select(xwaveid, paste(waveletter,"hhhqivw",sep=""), paste(waveletter,"hgage1",sep=""), paste(waveletter,"hgsex1",sep=""),paste(waveletter,"firisk",sep="")) %>%
      dplyr::rename(ID=xwaveid, inter_date=paste(waveletter,"hhhqivw",sep=""), age=paste(waveletter,"hgage1",sep=""), Gender=paste(waveletter,"hgsex1",sep=""), risk=paste(waveletter,"firisk",sep="")) %>%
      mutate(inter_date=strptime(inter_date, "%d/%m/%Y")) %>%
      mutate(SYEAR= SYEAR[i]) 
      input$risk[which(input$risk==5)]<-NA
  }else{
    input <- input %>%
      dplyr::select(xwaveid, paste(waveletter,"hhhqivw",sep=""), paste(waveletter,"hgage1",sep=""), paste(waveletter,"hgsex1",sep=""),paste(waveletter,"firisk",sep=""), paste(waveletter,"firiska",sep="")) %>%
      dplyr::rename(ID=xwaveid, inter_date=paste(waveletter,"hhhqivw",sep=""), age=paste(waveletter,"hgage1",sep=""), Gender=paste(waveletter,"hgsex1",sep=""), risk=paste(waveletter,"firisk",sep=""), risk_assumed=paste(waveletter,"firiska",sep="")) %>%
      mutate(inter_date=strptime(inter_date, "%d/%m/%Y")) %>%
      mutate(SYEAR= SYEAR[i]) 
      input$risk[which(input$risk==5)]<-input$risk_assumed[which(input$risk==5)]
  }
  assign(wave_name, value = input)
}

### combine all waves data
### preprocess and clean data
HILDA_preprocess <- Reduce(function(x, y) merge(x, y, all=TRUE), list(wave1, wave2, wave3, wave4, wave6, wave8, wave10, wave11, wave12, wave13, wave14, wave15, wave16, wave17, wave18, wave19)) %>%
  dplyr::select(ID, SYEAR, age, Gender, risk) %>%
  dplyr::filter(!is.na(Gender)) %>%
  dplyr::filter(!is.na(age)) %>%
  dplyr::filter(!is.na(risk)) %>%
  rename(Financial_raw=risk) %>%
  mutate(Financial_reverse = 5-Financial_raw) %>%
  mutate(Financial = 10*(Financial_reverse-1)/(4-1)) %>%
  dplyr::filter(age >= 18 & age <= 90) 

### check Gender and delete participants have different Gender
HILDA_ID_Gender <- HILDA_preprocess %>%
  dplyr::select(ID, Gender)

unique_HILDA_ID_Gender <-  unique(HILDA_ID_Gender) # get unique rows: unique function can eliminate duplicate rows
dupIDs <- with(unique_HILDA_ID_Gender, ID[duplicated(ID)])  # duplicate IDs, which means diffferent Gender: duplicated function can be used to identify duplicate ids after duplicate rows are removed
errorHILDA <- subset(HILDA_preprocess, ID %in% dupIDs)  
rightHILDA <- subset(HILDA_preprocess, !(ID %in% dupIDs)) 

### just keep participants >=2 records
dup_HILDA <- rightHILDA %>%
  dplyr::select(ID, age, Gender, SYEAR) %>%
  filter(!is.na(Gender)) %>%
  group_by(ID) %>%
  filter(n()>=2) %>%
  ungroup()

dup_HILDA_ID <- unique(dup_HILDA$ID)
HILDA_process1 <- subset(rightHILDA, !(ID %in% dup_HILDA_ID)) 
HILDA_process2 <- subset(rightHILDA, (ID %in% dup_HILDA_ID)) 

### cleaned data
HILDA_process <- HILDA_process2

### save 
setwd("~/Desktop/analysis/meta_analysis_risk_normalized_5/data/process_data")
saveRDS(HILDA_process, file = "HILDA_process.Rds")





### set age group
agebins = c(18, seq(25, 90, 5))
HILDA_process$age_bins <- cut(HILDA_process$age, breaks=agebins, include.lowest = TRUE, right = TRUE, lable = TRUE)

HILDA_process <- HILDA_process %>%
  dplyr::filter(!is.na(age_bins)) 
  
HILDA_process_reference <- HILDA_process %>%
  dplyr::filter(age_bins== "(50,55]")

mean_Financial <- mean(HILDA_process_reference$Financial_reverse, na.rm = TRUE)
sd_Financial <- sd(HILDA_process_reference$Financial_reverse, na.rm = TRUE)

HILDA_process_z <- HILDA_process %>%
  mutate(Financial_z = (Financial_reverse - mean_Financial)/sd_Financial)

# Saving & load XXX object in Rds format
setwd("~/Desktop/analysis/meta_analysis_risk_normalized_5/data/process_z_data")
saveRDS(HILDA_process_z, file = "HILDA_process_z.Rds")

#HILDA_process_z <- readRDS(file = "HILDA_process_z.Rds")




