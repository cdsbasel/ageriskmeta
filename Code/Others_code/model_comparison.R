### model comparision ###
rm(list=ls())
library(tidyverse)
library(lme4)

setwd("../data/process_z_data")

### prepare some loop variable ###
panels <- c("DHS","GCOE_Japan","GCOE_USA","GLES","HILDA","HRS","LIKS","PHF","SAVE",
            "SHARE_Austria","SHARE_Belgium","SHARE_Czech_Republic","SHARE_Denmark", "SHARE_Estonia", "SHARE_France", "SHARE_Germany","SHARE_Israel", "SHARE_Italy","SHARE_Netherlands","SHARE_Slovenia","SHARE_Spain","SHARE_Sweden","SHARE_Switzerland",
            "SOEP","UAS","USoc")   # do not include Addhealth / NLSY79_Child / NLSY79_YA panel
domains <- c("General", "Financial","Driving", "Recreational", "Occupational", "Health", "Social")
ab_domains <- c("g", "f", "d", "r", "o", "h", "s")
models = c("int", "fixed", "lg", "lg.gender", "lg.inter", "quad", "quad.gender")
model_plot <- models[3:7]


#step 1: create a empty model list(model_list) for every panel
model_list <- vector("list", length = length(panels))
names(model_list) <- panels
for (a in 1:length(panels)){
  panel_name <- panels[a]
  model_list[[a]] <- vector("list", length = length(ab_domains)*length(models))
  index=0
  model_name_list <- vector("list", length = length(ab_domains)*length(models))
  for (b in 1:length(ab_domains)){
    for (c in 1: length(models)) {
      index=index+1
      model_name_list[[index]] = paste0(ab_domains[b],".", models[c])
    }
    }
  names(model_list[[a]]) <- model_name_list
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
        dplyr::select(ID, age, Gender, SYEAR, domain_name) %>%
        filter(!is.na(get(domain_name)))  %>%
        group_by(ID) %>%
        filter(n()>=2) %>%
        ungroup() %>%
        drop_na() %>%
        mutate(c_age=age-50) %>%   #centered age: age-mean(age)  mean.age = 50.06019
        mutate(d_c_age=c_age/10)  #change age to decade age: divided by 10
      dataset[,"gender"] <- ifelse(dataset$Gender == 2, 1, 0)
      waves <- length(unique(dataset$SYEAR))
      # create models
      for (c in 1:length(models)) {
        model_name <- paste0(ab_domains[b],".", models[c])
        print(model_name)
        #unconditional means model/intercept-only model/null model
        if(c==1){
          model_list[[panel_name]][[model_name]] <- lmer(get(domain_name) ~ (1|ID), data= dataset, REML=F,
                                                         control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
          print(model_name)
        }
        #unconditional growth model with a fixed slope(fixed)
        if(c==2){
          model_list[[panel_name]][[model_name]] <- lmer(get(domain_name) ~ d_c_age + (1|ID), data=dataset, REML=F,
                                                         control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
          print(model_name)
        }
        #unconditional growth model with a fixed/random slope(lg)
        if(c==3){
          model_list[[panel_name]][[model_name]] <- lmer(get(domain_name) ~ d_c_age + (1 + d_c_age|ID), data=dataset, REML=F,
                                                         control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
          print(model_name)
        }
        #linear growth, with gender(lg.gender)
        if(c==4){
          model_list[[panel_name]][[model_name]] <- lmer(get(domain_name) ~ d_c_age + gender + (1+d_c_age|ID), data=dataset,REML=F, 
                                           control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
          print(model_name)
        }
        #linaer growth model, with interaction(lg.inter)
        if(c==5){
          model_list[[panel_name]][[model_name]] <- lmer(get(domain_name) ~ d_c_age + d_c_age*gender + (1+d_c_age|ID), data=dataset,REML=F, 
                                                         control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
          print(model_name)
        }
        
        #quadratic growth model(quad)
        if(c==6){
          if(waves >3){
            model_list[[panel_name]][[model_name]] <- lmer(get(domain_name) ~ d_c_age + I(d_c_age^2) +(1+d_c_age|ID), data=dataset, REML=F,
                                                           control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
            print(model_name)
          }
        }
        #quadratic growth model, with gender (quad.gender)
        if(c==7){
          if(waves >3){
            model_list[[panel_name]][[model_name]] <- lmer(get(domain_name) ~ d_c_age + I(d_c_age^2) + gender + (1+d_c_age|ID), data=dataset,
                                             REML=F, control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
            print(model_name)
          }
        }
      }
    }
  }
}


### save results ###
setwd("../data")
saveRDS(model_list, file ="results_data/model_list.Rds")


#step 3: model comparison
#General
General_results <- model_list$GCOE_Japan
anova(General_results$g.int, General_results$g.fixed, General_results$g.lg, General_results$g.lg.gender, General_results$g.lg.inter)  # g.lg < g.lg.inter < g.lg.gender     *g.lg.gender
anova(General_results$g.int, General_results$g.fixed, General_results$g.lg, General_results$g.lg.gender, General_results$g.lg.inter, General_results$g.quad, General_results$g.quad.gender)  # g.quad < g.quad.gender   *g.lg.gender

General_results <- model_list$GCOE_USA
anova(General_results$g.int, General_results$g.fixed, General_results$g.lg, General_results$g.lg.gender, General_results$g.lg.inter)  # g.lg < g.lg.inter ~ g.lg.gender    *g.lg.gender
anova(General_results$g.int, General_results$g.fixed, General_results$g.lg, General_results$g.lg.gender, General_results$g.lg.inter, General_results$g.quad, General_results$g.quad.gender)  # g.quad < g.quad.gender   *g.lg.gender

### when 4 waves
General_results <- model_list$GLES
anova(General_results$g.int, General_results$g.fixed, General_results$g.lg, General_results$g.lg.gender, General_results$g.lg.inter) # g.lg < g.lg.inter ~ g.lg.gender    *g.lg.gender
anova(General_results$g.int, General_results$g.fixed, General_results$g.lg, General_results$g.lg.gender, General_results$g.lg.inter, General_results$g.quad, General_results$g.quad.gender)   # g.quad < g.quad.gender    *g.lg.gender

General_results <- model_list$HRS
anova(General_results$g.int, General_results$g.fixed, General_results$g.lg, General_results$g.lg.gender, General_results$g.lg.inter)  # g.lg < g.lg.inter < g.lg.gender   *g.lg.gender
anova(General_results$g.int, General_results$g.fixed, General_results$g.lg, General_results$g.lg.gender, General_results$g.lg.inter, General_results$g.quad, General_results$g.quad.gender)  # g.quad < g.quad.gender   *g.lg.gender ###new

General_results <- model_list$LIKS
anova(General_results$g.int, General_results$g.fixed, General_results$g.lg, General_results$g.lg.gender, General_results$g.lg.inter)   # g.lg < g.lg.gender < g.lg.inter     *g.lg.inter
anova(General_results$g.int, General_results$g.fixed, General_results$g.lg, General_results$g.lg.gender, General_results$g.lg.inter, General_results$g.quad, General_results$g.quad.gender)   # g.quad < g.quad.gender   *g.quad.gender 

General_results <- model_list$PHF 
anova(General_results$g.int, General_results$g.fixed, General_results$g.lg, General_results$g.lg.gender, General_results$g.lg.inter)  #  g.lg.inter < g.lg.gender ~ g.lg   *g.lg

General_results <- model_list$UAS
anova(General_results$g.int, General_results$g.fixed, General_results$g.lg, General_results$g.lg.gender, General_results$g.lg.inter)  #g.lg < g.lg.inter ~ g.lg.gender     *g.lg.gender
anova(General_results$g.int, General_results$g.fixed, General_results$g.lg, General_results$g.lg.gender, General_results$g.lg.inter, General_results$g.quad, General_results$g.quad.gender)  # g.quad < g.quad.gender   *g.lg.gender

General_results <- model_list$USoc
anova(General_results$g.int, General_results$g.fixed, General_results$g.lg, General_results$g.lg.gender, General_results$g.lg.inter)  # g.lg < g.lg.inter < g.lg.gender    *g.lg.gender

General_results <- model_list$SOEP
anova(General_results$g.int, General_results$g.fixed, General_results$g.lg, General_results$g.lg.gender, General_results$g.lg.inter)  #g.lg < g.lg.inter < g.lg.gender     *g.lg.gender
anova(General_results$g.int, General_results$g.fixed, General_results$g.lg, General_results$g.lg.gender, General_results$g.lg.inter, General_results$g.quad, General_results$g.quad.gender)  # g.quad < g.quad.gender   g.quad.gender




#Financial
Financial_results <- model_list$DHS
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter) # f.lg < f.lg.inter ~ f.lg.gender    * f.lg.gender
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter, Financial_results$f.quad,Financial_results$f.quad.gender)  # f.quad < f.quad.gender   * f.quad.gender

Financial_results <- model_list$HILDA
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter)  # f.lg < f.lg.inter < f.lg.gender   * f.lg.gender    
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter, Financial_results$f.quad,Financial_results$f.quad.gender)  # f.quad < f.quad.gender   * f.quad.gender

Financial_results <- model_list$HRS
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter) # f.lg < f.lg.inter ~ f.lg.gender   * f.lg.gender
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter, Financial_results$f.quad,Financial_results$f.quad.gender)   # f.quad < f.quad.gender   * f.lg.gender  ### new

Financial_results <- model_list$PHF
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter) # f.lg < f.lg.inter < f.lg.gender   * f.lg.gender

Financial_results <- model_list$SAVE
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter) # f.lg < f.lg.gender < f.lg.inter   * f.lg.inter
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter, Financial_results$f.quad,Financial_results$f.quad.gender)  # f.quad < f.quad.gender    * f.lg.inter

Financial_results <- model_list$SHARE_Austria
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter) # f.lg < f.lg.inter ~ f.lg.gender    * f.lg.gender
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter, Financial_results$f.quad,Financial_results$f.quad.gender)  # f.quad < f.quad.gender    * f.lg.gender

Financial_results <- model_list$SHARE_Belgium
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter)  # f.lg < f.lg.inter < f.lg.gender   * f.lg.gender      
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter, Financial_results$f.quad,Financial_results$f.quad.gender)  # f.quad < f.quad.gender     * f.lg.gender

Financial_results <- model_list$SHARE_Czech_Republic
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter)  # f.lg < f.lg.inter < f.lg.gender   * f.lg.gender   
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter, Financial_results$f.quad,Financial_results$f.quad.gender)  # f.quad < f.quad.gender     * f.lg.gender

Financial_results <- model_list$SHARE_Denmark
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter)  # f.lg < f.lg.inter ~ f.lg.gender   * f.lg.gender    
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter, Financial_results$f.quad,Financial_results$f.quad.gender)  # f.quad < f.quad.gender     * f.lg.gender

Financial_results <- model_list$SHARE_Estonia
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter)  # f.lg < f.lg.gender < f.lg.inter   * f.lg.inter 

Financial_results <- model_list$SHARE_France
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter)  # f.lg < f.lg.inter < f.lg.gender    * f.lg.gender   
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter, Financial_results$f.quad,Financial_results$f.quad.gender)  # f.quad < f.quad.gender    * f.lg.gender

Financial_results <- model_list$SHARE_Germany
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter) # f.lg < f.lg.inter < f.lg.gender    * f.lg.gender 
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter, Financial_results$f.quad,Financial_results$f.quad.gender)  # f.quad < f.quad.gender   * f.lg.gender

Financial_results <- model_list$SHARE_Israel
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter)  # f.lg < f.lg.inter < f.lg.gender   * f.lg.gender   

Financial_results <- model_list$SHARE_Italy
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter)  # f.lg < f.lg.inter < f.lg.gender   * f.lg.gender   
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter, Financial_results$f.quad,Financial_results$f.quad.gender)  # f.quad < f.quad.gender    * f.lg.gender

Financial_results <- model_list$SHARE_Netherlands
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter)  # f.lg < f.lg.gender < f.lg.inter   * f.lg.inter 
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter, Financial_results$f.quad,Financial_results$f.quad.gender)  # f.quad < f.quad.gender    * f.lg.inter 

Financial_results <- model_list$SHARE_Slovenia
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter)  # f.lg < f.lg.inter ~ f.lg.gender   * f.lg.gender    
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter, Financial_results$f.quad,Financial_results$f.quad.gender)  # f.quad < f.quad.gender    * f.lg.gender

Financial_results <- model_list$SHARE_Spain
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter)  # f.lg < f.lg.inter < f.lg.gender    * f.lg.gender   
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter, Financial_results$f.quad,Financial_results$f.quad.gender)  # f.quad < f.quad.gender   * f.quad.gender

Financial_results <- model_list$SHARE_Sweden
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter) # f.lg < f.lg.inter < f.lg.gender    * f.lg.gender  
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter, Financial_results$f.quad,Financial_results$f.quad.gender)  # f.quad < f.quad.gender   * f.lg.gender  

Financial_results <- model_list$SHARE_Switzerland
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter)  # f.lg < f.lg.inter < f.lg.gender    * f.lg.gender    
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter, Financial_results$f.quad,Financial_results$f.quad.gender)  # f.quad < f.quad.gender     * f.lg.gender

Financial_results <- model_list$SOEP
anova(Financial_results$f.int, Financial_results$f.fixed, Financial_results$f.lg, Financial_results$f.lg.gender, Financial_results$f.lg.inter) # f.lg < f.lg.gender ~ f.lg.inter   * f.lg.gender





#Driving
Driving_results <- model_list$HRS
anova(Driving_results$d.int, Driving_results$d.fixed, Driving_results$d.lg, Driving_results$d.lg.gender, Driving_results$d.lg.inter) # d.lg < d.lg.inter < d.lg.gender   *d.lg.gender
anova(Driving_results$d.int, Driving_results$d.fixed, Driving_results$d.lg, Driving_results$d.lg.gender, Driving_results$d.lg.inter, Driving_results$d.quad, Driving_results$d.quad.gender)  # d.quad < d.quad.gender  *d.lg.gender   ###new

Driving_results <- model_list$SAVE
anova(Driving_results$d.int, Driving_results$d.fixed, Driving_results$d.lg, Driving_results$d.lg.gender, Driving_results$d.lg.inter) # d.lg < d.lg.gender < d.lg.inter   *d.lg.inter
anova(Driving_results$d.int, Driving_results$d.fixed, Driving_results$d.lg, Driving_results$d.lg.gender, Driving_results$d.lg.inter, Driving_results$d.quad, Driving_results$d.quad.gender)   # d.quad < d.quad.gender   *d.lg.inter

Driving_results <- model_list$SOEP
anova(Driving_results$d.int, Driving_results$d.fixed, Driving_results$d.lg, Driving_results$d.lg.gender, Driving_results$d.lg.inter) # d.lg < d.lg.inter ~ d.lg.gender   *d.lg.gender


#Recreational
Recreational_results <- model_list$HRS
anova(Recreational_results$r.int, Recreational_results$r.fixed, Recreational_results$r.lg, Recreational_results$r.lg.gender, Recreational_results$r.lg.inter)  # r.lg < r.lg.inter < r.lg.gender     * r.lg.gender
anova(Recreational_results$r.int, Recreational_results$r.fixed, Recreational_results$r.lg, Recreational_results$r.lg.gender, Recreational_results$r.lg.inter, Recreational_results$r.quad, Recreational_results$r.quad.gender) # r.quad < r.quad.gender  * r.lg.gender  ###new

Recreational_results <- model_list$SAVE
anova(Recreational_results$r.int, Recreational_results$r.fixed, Recreational_results$r.lg, Recreational_results$r.lg.gender, Recreational_results$r.lg.inter)  # r.lg < r.lg.gender < r.lg.inter     * r.lg.inter
anova(Recreational_results$r.int, Recreational_results$r.fixed, Recreational_results$r.lg, Recreational_results$r.lg.gender, Recreational_results$r.lg.inter, Recreational_results$r.quad, Recreational_results$r.quad.gender) # r.quad < r.quad.gender    * r.quad.gender 

Recreational_results <- model_list$SOEP
anova(Recreational_results$r.int, Recreational_results$r.fixed, Recreational_results$r.lg, Recreational_results$r.lg.gender, Recreational_results$r.lg.inter)  #  r.lg < r.lg.gender < r.lg.inter    * r.lg.inter



#Occupational
Occupational_results <- model_list$HRS
anova(Occupational_results$o.int, Occupational_results$o.fixed, Occupational_results$o.lg, Occupational_results$o.lg.gender, Occupational_results$o.lg.inter)  # o.lg < o.lg.inter ~ o.lg.gender   * o.lg.gender
anova(Occupational_results$o.int, Occupational_results$o.fixed, Occupational_results$o.lg, Occupational_results$o.lg.gender, Occupational_results$o.lg.inter, Occupational_results$o.quad, Occupational_results$o.quad.gender)  # o.quad < o.quad.gender    * o.lg.gender  ###new

Occupational_results <- model_list$SAVE
anova(Occupational_results$o.int, Occupational_results$o.fixed, Occupational_results$o.lg, Occupational_results$o.lg.gender, Occupational_results$o.lg.inter)  # o.lg < o.lg.inter ~ o.lg.gender   * o.lg.gender
anova(Occupational_results$o.int, Occupational_results$o.fixed, Occupational_results$o.lg, Occupational_results$o.lg.gender, Occupational_results$o.lg.inter, Occupational_results$o.quad, Occupational_results$o.quad.gender)  # o.quad < o.quad.gender    * o.quad.gender

Occupational_results <- model_list$SOEP
anova(Occupational_results$o.int, Occupational_results$o.fixed, Occupational_results$o.lg, Occupational_results$o.lg.gender, Occupational_results$o.lg.inter)  # o.lg < o.lg.inter ~ o.lg.gender   * o.lg.gender


#Health
Health_results <- model_list$HRS
anova(Health_results$h.int, Health_results$h.fixed, Health_results$h.lg, Health_results$h.lg.gender, Health_results$h.lg.inter)  # h.lg < h.lg.inter ~ h.lg.gender     *h.lg.gender
anova(Health_results$h.int, Health_results$h.fixed, Health_results$h.lg, Health_results$h.lg.gender, Health_results$h.lg.inter, Health_results$h.quad, Health_results$h.quad.gender) # h.quad < h.quad.gender  *h.lg.gender  ###new

Health_results <- model_list$SAVE
anova(Health_results$h.int, Health_results$h.fixed, Health_results$h.lg, Health_results$h.lg.gender, Health_results$h.lg.inter)  # h.lg <  h.lg.gender < h.lg.inter     *h.lg.inter
anova(Health_results$h.int, Health_results$h.fixed, Health_results$h.lg, Health_results$h.lg.gender, Health_results$h.lg.inter, Health_results$h.quad, Health_results$h.quad.gender)   # h.quad < h.quad.gender  **h.lg.inter

Health_results <- model_list$SOEP
anova(Health_results$h.int, Health_results$h.fixed, Health_results$h.lg, Health_results$h.lg.gender, Health_results$h.lg.inter)  # h.lg < h.lg.gender < h.lg.inter    *h.lg.inter



#Social
Social_results <- model_list$SOEP
anova(Social_results$s.int, Social_results$s.fixed, Social_results$s.lg, Social_results$s.lg.gender, Social_results$s.lg.inter)  # s.lg < s.lg.inter < s.lg.gender    









  





        