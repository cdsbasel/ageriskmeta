### model comparision ###
rm(list=ls())
library(tidyverse)
library(lme4)

### set working directory
setwd(".../data/process_z_data")

### prepare some loop variable ###
panels <- c("DHS","GCOE_Japan","GCOE_USA","GLES","HILDA","HRS","LIKS","PHF","SAVE","SHARE_Austria","SHARE_Belgium","SHARE_Czech_Republic","SHARE_Denmark", "SHARE_Estonia", "SHARE_France", "SHARE_Germany","SHARE_Israel", "SHARE_Italy","SHARE_Netherlands","SHARE_Slovenia","SHARE_Spain","SHARE_Sweden","SHARE_Switzerland","SOEP","UAS","USoc")   # do not include Addhealth / NLSY79_Child / NLSY79_YA panel
domains <- c("General", "Financial","Driving", "Recreational", "Occupational", "Health", "Social")
ab_domains <- c("g", "f", "d", "r", "o", "h", "s")
models = c("lg.gender.group", "lg.gender.group.inter")



#step 1: create a empty model list(model_list_cohort) for every panel
model_list_cohort <- vector("list", length = length(panels))
names(model_list_cohort) <- panels
for (a in 1:length(panels)){
  panel_name <- panels[a]
  model_list_cohort[[a]] <- vector("list", length = length(ab_domains)*length(models))
  index=0
  model_name_list <- vector("list", length = length(ab_domains)*length(models))
  for (b in 1:length(ab_domains)){
    for (c in 1: length(models)) {
      index=index+1
      model_name_list[[index]] = paste0(ab_domains[b],".", models[c])
    }
  }
  names(model_list_cohort[[a]]) <- model_name_list
}


#step 2:create model and put that into empty model list
for (a in 1:length(panels)){
  panel_name <- panels[a]
  print(panel_name)
  panel <- readRDS(paste0(panels[a], "_process_z.Rds")) #load all panel data one by one
  for(b in 1:length(domains)){
    domain_name <- paste0(domains[b], "_z")
    print(domain_name)
    if(domain_name %in% names(panel)){
      dataset <- panel %>%
        dplyr::select(ID, age, Gender, SYEAR, all_of(domain_name)) %>%
        dplyr::filter(!is.na(get(domain_name)))  %>%
        dplyr::group_by(ID) %>%
        dplyr::filter(n()>=2) %>%
        ungroup() %>%
        drop_na() %>%
        mutate(c.age=age-50) %>%   #centered age: age-mean(age)  mean.age = 50.06019
        mutate(d.c.age=c.age/10) %>% #change age to decade age: divided by 10
        group_by(ID) %>%
        mutate(min.age = min(age)) %>%
        ungroup() %>%
        mutate(age.group = if_else(min.age < 60, 0, 1)) %>%
        mutate(gender = if_else(Gender == 2, 1, 0)) 
      waves <- length(unique(dataset$SYEAR))
      
      # create models
      for (c in 1:length(models)) {
        model_name <- paste0(ab_domains[b],".", models[c])
        print(model_name)
        
        # lg.gender.group model
        if(c==1){
          model_list_cohort[[panel_name]][[model_name]] <- lmer(get(domain_name) ~ d.c.age + gender + age.group + (1+d.c.age|ID), data=dataset,REML=F, 
                                           control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
          print(model_name)
        }
        
        #lg.gender.group.inter model
        if(c==2){
          model_list_cohort[[panel_name]][[model_name]] <- lmer(get(domain_name) ~ d.c.age + gender + age.group + d.c.age * gender * age.group + (1+d.c.age|ID), data=dataset,REML=F, 
                                           control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
          print(model_name)
        }
      }
    }
  }
}


### save results ###
setwd("/Users/yliu/Desktop/analysis/meta_analysis_risk_normalized_5/data")
saveRDS(model_list_cohort, file ="results_data/model_list_cohort.Rds")


#step 3: model comparison
#General: 7 g.lg.gender.group, 1 g.lg.gender.group.inter
General_results <- model_list_cohort$GCOE_Japan
anova(General_results$g.lg.gender.group, General_results$g.lg.gender.group.inter)  #  g.lg.gender.group.inter < g.lg.gender.group 

General_results <- model_list_cohort$GCOE_USA
anova(General_results$g.lg.gender.group, General_results$g.lg.gender.group.inter)  #  g.lg.gender.group.inter ~ g.lg.gender.group

General_results <- model_list_cohort$GLES
anova(General_results$g.lg.gender.group, General_results$g.lg.gender.group.inter)  #   g.lg.gender.group < g.lg.gender.group.inter

General_results <- model_list_cohort$HRS
anova(General_results$g.lg.gender.group, General_results$g.lg.gender.group.inter)  #  g.lg.gender.group.inter < g.lg.gender.group

General_results <- model_list_cohort$LIKS
anova(General_results$g.lg.gender.group, General_results$g.lg.gender.group.inter)  #  g.lg.gender.group.inter ~ g.lg.gender.group

General_results <- model_list_cohort$PHF
anova(General_results$g.lg.gender.group, General_results$g.lg.gender.group.inter)  #  g.lg.gender.group.inter ~ g.lg.gender.group

General_results <- model_list_cohort$UAS
anova(General_results$g.lg.gender.group, General_results$g.lg.gender.group.inter)  #  g.lg.gender.group.inter < g.lg.gender.group

General_results <- model_list_cohort$USoc
anova(General_results$g.lg.gender.group, General_results$g.lg.gender.group.inter)  #  g.lg.gender.group.inter < g.lg.gender.group

General_results <- model_list_cohort$SOEP
anova(General_results$g.lg.gender.group, General_results$g.lg.gender.group.inter)  #  g.lg.gender.group.inter < g.lg.gender.group




#Financial: 16 f.lg.gender.group, 4 f.lg.gender.group.inter
Financial_results <- model_list_cohort$DHS
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group < f.lg.gender.group.inter

Financial_results <- model_list_cohort$HILDA
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group < f.lg.gender.group.inter

Financial_results <- model_list_cohort$HRS
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group < f.lg.gender.group.inter

Financial_results <- model_list_cohort$PHF
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group.inter < f.lg.gender.group  

Financial_results <- model_list_cohort$SAVE
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group < f.lg.gender.group.inter

Financial_results <- model_list_cohort$SHARE_Austria
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group.inter < f.lg.gender.group

Financial_results <- model_list_cohort$SHARE_Belgium
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group.inter < f.lg.gender.group  

Financial_results <- model_list_cohort$SHARE_Czech_Republic
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group.inter < f.lg.gender.group 

Financial_results <- model_list_cohort$SHARE_Denmark
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group.inter ~ f.lg.gender.group 

Financial_results <- model_list_cohort$SHARE_Estonia
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group.inter < f.lg.gender.group 

Financial_results <- model_list_cohort$SHARE_France
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group.inter ~ f.lg.gender.group 

Financial_results <- model_list_cohort$SHARE_Germany
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group.inter < f.lg.gender.group 

Financial_results <- model_list_cohort$SHARE_Israel
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group.inter < f.lg.gender.group 

Financial_results <- model_list_cohort$SHARE_Italy
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group.inter < f.lg.gender.group 

Financial_results <- model_list_cohort$SHARE_Netherlands
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group.inter ~ f.lg.gender.group 

Financial_results <- model_list_cohort$SHARE_Slovenia
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group.inter < f.lg.gender.group 

Financial_results <- model_list_cohort$SHARE_Spain
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group.inter < f.lg.gender.group 

Financial_results <- model_list_cohort$SHARE_Sweden
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group.inter < f.lg.gender.group 

Financial_results <- model_list_cohort$SHARE_Switzerland
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group.inter < f.lg.gender.group 

Financial_results <- model_list_cohort$SOEP
anova(Financial_results$f.lg.gender.group, Financial_results$f.lg.gender.group.inter)  # f.lg.gender.group.inter ~ f.lg.gender.group




#Driving
Driving_results <- model_list_cohort$HRS
anova(Driving_results$d.lg.gender.group, Driving_results$d.lg.gender.group.inter) # d.lg.gender.group.inter < d.lg.gender.group

Driving_results <- model_list_cohort$SAVE
anova(Driving_results$d.lg.gender.group, Driving_results$d.lg.gender.group.inter) # d.lg.gender.group < d.lg.gender.group.inter

Driving_results <- model_list_cohort$SOEP
anova(Driving_results$d.lg.gender.group, Driving_results$d.lg.gender.group.inter) # d.lg.gender.group.inter < d.lg.gender.group



#Recreational
Recreational_results <- model_list_cohort$HRS
anova(Recreational_results$r.lg.gender.group, Recreational_results$r.lg.gender.group.inter) # r.lg.gender.group.inter < r.lg.gender.group

Recreational_results <- model_list_cohort$SAVE      
anova(Recreational_results$r.lg.gender.group, Recreational_results$r.lg.gender.group.inter)  #  r.lg.gender.group < r.lg.gender.group.inter

Recreational_results <- model_list_cohort$SOEP
anova(Recreational_results$r.lg.gender.group, Recreational_results$r.lg.gender.group.inter) # r.lg.gender.group.inter ~ r.lg.gender.group



#Occupational
Occupational_results <- model_list_cohort$HRS
anova(Occupational_results$o.lg.gender.group, Occupational_results$o.lg.gender.group.inter)  # o.lg.gender.group.inter ~ o.lg.gender.group

Occupational_results <- model_list_cohort$SAVE
anova(Occupational_results$o.lg.gender.group, Occupational_results$o.lg.gender.group.inter)  # o.lg.gender.group.inter < o.lg.gender.group

Occupational_results <- model_list_cohort$SOEP
anova(Occupational_results$o.lg.gender.group, Occupational_results$o.lg.gender.group.inter)   # o.lg.gender.group.inter ~ o.lg.gender.group



#Health
Health_results <- model_list_cohort$HRS
anova(Health_results$h.lg.gender.group, Health_results$h.lg.gender.group.inter)    # h.lg.gender.group.inter ~ h.lg.gender.group 

Health_results <- model_list_cohort$SAVE
anova(Health_results$h.lg.gender.group, Health_results$h.lg.gender.group.inter)    # h.lg.gender.group.inter ~ h.lg.gender.group 

Health_results <- model_list_cohort$SOEP
anova(Health_results$h.lg.gender.group, Health_results$h.lg.gender.group.inter)    # h.lg.gender.group.inter < h.lg.gender.group 


#Social
Social_results <- model_list_cohort$SOEP
anova(Social_results$s.lg.gender.group, Social_results$s.lg.gender.group.inter)   # s.lg.gender.group.inter < s.lg.gender.group   









  





        