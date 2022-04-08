##### meta analysis for all parameters ######
rm(list=ls())
library(metafor)
library(tidyverse)

setwd("/Users/yliu/Desktop/analysis/meta_analysis_risk_normalized_5/data")
results_table_list <- readRDS(file = "results_data/results_table_list.Rds")

respondents <- read_csv(file = "results_data/respondents.csv") %>%
  select("...1", "General", "Financial", "Driving", "Recreational", "Occupational", "Health", "Social", "total")

meanage <- read_csv(file = "results_data/meanage.csv") %>%
  mutate_if(is.numeric, round, digits=2)

mean.age = meanage$total
meanage.general <- meanage$General
meanage.financial <- meanage$Financial

### prepare some loop variable ###
panels <- c("DHS","GCOE_Japan","GCOE_USA","GLES","HILDA","HRS","LIKS","PHF","SAVE",
            "SHARE_Austria","SHARE_Belgium","SHARE_Czech_Republic","SHARE_Denmark", "SHARE_Estonia", "SHARE_France", "SHARE_Germany","SHARE_Israel", "SHARE_Italy","SHARE_Netherlands","SHARE_Slovenia","SHARE_Spain","SHARE_Sweden","SHARE_Switzerland",
            "SOEP","UAS","USoc")   # do not include Addhealth / NLSY79_Child / NLSY79_YA panel
domains <- c("General", "Financial")   #just do meta-analysis with all moderator for general and financial domain, since other domains only include 3 panels
ab_domains <- c("g", "f")
models = c("int", "fixed", "lg", "lg.gender", "lg.inter", "quad", "quad.gender")

research <- c(1, rep(2, 2), 3, 4, 5, 6, 7, 8,
              rep(9, 14),
              10, 11, 12)

country = c("Netherlands","Japan","USA","Germany","Australia","USA","Kyrgyzstan","Germany","Germany",
            "Austria","Belgium","Czech Republic","Denmark","Estonia","France","Germany","Israel","Italy","Netherlands","Slovenia","Spain","Sweden","Switzerland",
            "Germany","USA","UK")
continent = c("Europe","Asia","North America","Europe","Oceania","North America","Asia","Europe","Europe",
              "Europe","Europe","Europe","Europe","Europe","Europe","Europe","Asia","Europe","Europe","Europe","Europe","Europe","Europe",
              "Europe","North America","Europe")



scale.general = c(7, 11, 11,11, 4, 11, 11, 11, 7,
                rep(4, 14), 
                11, 11, 11)

scale.financial = c(7, 11, 11,11, 4, 11, 11, 4, 7,
                  rep(4, 14), 
                  11, 11, 11)


cofficients1 <- list(list("icc", "icc.se", "icc_N.obs"),  # int
                    list("age_Estimate", "age_Std.Error1", "age_N.obs"),  # fixed
                    list("age_Estimate", "age_Std.Error1", "age_N.obs"),  # lg
                    list("age_Estimate", "age_Std.Error1", "age_N.obs",   "gender_Estimate", "gender_Std.Error1", "gender_N.obs"),  # lg.gender
                    list("age_Estimate", "age_Std.Error1", "age_N.obs",   "gender_Estimate", "gender_Std.Error1", "gender_N.obs",   "age_gender_Estimate", "age_gender_Std.Error1", "age_gender_N.obs"),  # lg.inter
                    list("age_Estimate", "age_Std.Error1", "age_N.obs",   "age2_Estimate", "age2_Std.Error1", "age2_N.obs"),  # quad
                    list("age_Estimate", "age_Std.Error1", "age_N.obs",   "age2_Estimate", "age2_Std.Error1", "age2_N.obs",  "gender_Estimate", "gender_Std.Error1", "gender_N.obs"))  # quad.gender

cofficients2 <- list(list("icc", "icc.se", "N.obs"),  # int
                     list("d_c_age_Estimate", "d_c_age_Std.Error1", "N.obs"),  # fixed
                     list("d_c_age_Estimate", "d_c_age_Std.Error1", "N.obs"),  # lg
                     list("d_c_age_Estimate", "d_c_age_Std.Error1", "N.obs",  "gender_Estimate", "gender_Std.Error1", "N.obs"), # lg.gender
                     list("d_c_age_Estimate", "d_c_age_Std.Error1", "N.obs",  "gender_Estimate", "gender_Std.Error1", "N.obs",  "d_c_age:gender_Estimate", "d_c_age:gender_Std.Error1", "N.obs"),  # lg.inter
                     list("d_c_age_Estimate", "d_c_age_Std.Error1", "N.obs",  "I(d_c_age^2)_Estimate", "I(d_c_age^2)_Std.Error1", "N.obs"), # quad
                     list("d_c_age_Estimate", "d_c_age_Std.Error1", "N.obs",  "I(d_c_age^2)_Estimate", "I(d_c_age^2)_Std.Error1","N.obs",  "gender_Estimate", "gender_Std.Error1", "N.obs")) # quad.gender


cofficient_name <- list(list("icc"),   # int
                        list("age"),  # fixed
                        list("age"),  # lg
                        list("age", "gender"),  # lg.gender
                        list("age", "gender", "age_gender"), # lg.inter
                        list("age","age2"),  # quad
                        list("age", "age2", "gender"))  # quad.gender


### function 1: create a empty meta_data_list for saving selected meta-data
create_meta_data_list  <- function(models,ab_domains,panels, cofficients1){
  index=0
  results_name_list <- list()  
  meta_data_list <- vector("list", length = length(ab_domains)*length(models))
  for (b in 1:length(ab_domains)) {
    for (c in 1: length(models)) {
      index=index+1
      meta_data_list[[index]] <- data.frame(matrix(data=NA, nrow=length(cofficients1[[c]]), ncol = length(panels)))
      colnames(meta_data_list[[index]]) <- panels
      rownames(meta_data_list[[index]]) <- unlist(cofficients1[[c]])
      results_name_list[[index]] <- paste0("meta","_",ab_domains[b],".", models[c])
    }
  }
  names(meta_data_list) <- results_name_list
  return(meta_data_list)
}
meta_data_list <- create_meta_data_list(models,ab_domains,panels, cofficients1)


###function 2: extract meta analysis needed parameter from results_table_list and save them into meta_data_list
extract_meta_parameter <- function(ab_domains, models, coefficients1, cofficients2, meta_data_list, results_table_list){
  for (b in 1:length(ab_domains)) {
    for (c in 1:length(models)) {
      meta_model_name <- paste0("meta","_",ab_domains[b],".", models[c])
      results_model_name <- paste0("results","_",ab_domains[b],".", models[c])
        for(d in 1:length(cofficients1[[c]])){
          results_rowname <- cofficients2[[c]][[d]]
          meta_rowname <- cofficients1[[c]][[d]]
        meta_data_list[[meta_model_name]][meta_rowname,] <- results_table_list[[results_model_name]][results_rowname,]
        }
        meta_data_list[[meta_model_name]] <- as.data.frame(t(meta_data_list[[meta_model_name]]))
        meta_data_list[[meta_model_name]]$research <- as.factor(research)
        meta_data_list[[meta_model_name]]$country <- as.factor(country)
        meta_data_list[[meta_model_name]]$continent <- as.factor(continent)
        meta_data_list[[meta_model_name]]$scale.general <- scale.general
        meta_data_list[[meta_model_name]]$scale.financial <- scale.financial
        meta_data_list[[meta_model_name]] <- tibble::rownames_to_column(meta_data_list[[meta_model_name]], "study")
    }
  }
  return(meta_data_list)
}
meta_data_list <- extract_meta_parameter(ab_domains, models, coefficients1, cofficients2, meta_data_list, results_table_list)


### function 3: create a empty list to save meta-analysis results ###
create_meta_results_list <- function(ab_domains,models, cofficient_name){
  index=0
  results_name_list <- list()  
  meta_results_list <- vector("list", length = length(ab_domains)*length(models))
  for (b in 1:length(ab_domains)) {
    for (c in 1: length(models)) {
      index=index+1
      results_name_list[[index]] <- paste0("meta","_",ab_domains[b],".", models[c])
      meta_results_list[[index]] <- vector("list", length = length(cofficient_name[[c]]))
      for (d in 1:length(cofficient_name[[c]])){
      names(meta_results_list[[index]]) <- unlist(cofficient_name[[c]])
     }
    }
  }
  names(meta_results_list) <- results_name_list
  return(meta_results_list)
}

meta_results_list_all <- create_meta_results_list(ab_domains, models, cofficient_name) # including all 3 moderators(continent, mean age, scale range)


### function 4: do meta analysis and save the results into previous meta_results_list
# slab = study  note: slab means study labels, optional vector with labels for the studies.
meta_analysis <- function(ab_domains, models,cofficient_name, meta_data_list, meta_results_list_all) {
  for (b in 1:length(ab_domains)) {
    for (c in 1: length(models)) {
      meta_results_name <- paste0("meta","_",ab_domains[b],".", models[c])
      print(paste0("b=", b))
      print(paste0("c=", c))
      
      # int
      if(c==1){ 
        d = length(cofficient_name[[c]])  #d=1
        cofficient <- cofficient_name[[c]][[d]]
        print(cofficient)
        metadata <- meta_data_list[[meta_results_name]]
        if(b == 1){
          metadata$meanage.general <- meanage.general 
          metadata <- metadata %>%
            rename(mean.age = meanage.general, scale = scale.general) %>%
            drop_na()
          meta_results_list_all[[meta_results_name]][[cofficient]] <- rma (yi = icc,
                                                                           sei = icc.se,
                                                                           slab = study,
                                                                           mods = ~ continent + mean.age + scale,
                                                                           ni = icc_N.obs,
                                                                           data = metadata)
          }else{
            metadata$meanage.financial <- meanage.financial 
            metadata <- metadata %>%
              rename(mean.age = meanage.financial, scale = scale.financial) %>%
              drop_na()
            meta_results_list_all[[meta_results_name]][[cofficient]] <- rma (yi = icc,
                                                                             sei = icc.se,
                                                                             slab = study,
                                                                             mods = ~ continent + mean.age + scale,
                                                                             ni = icc_N.obs,
                                                                             data = metadata)
            }
        print("icc")
        }
      
      # fixed or lg
      if(c %in% 2:3){
        d = length(cofficient_name[[c]])  #d=1
        cofficient <- cofficient_name[[c]][[d]]
        print(cofficient)
        metadata <- meta_data_list[[meta_results_name]]
        if(b == 1){
          metadata$meanage.general <- meanage.general 
          metadata <- metadata %>%
            rename(mean.age = meanage.general, scale = scale.general) %>%
            drop_na()
          meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_Estimate,
                                                                          sei = age_Std.Error1,
                                                                          slab = study,
                                                                          mods = ~ continent + mean.age + scale,
                                                                          ni = age_N.obs,
                                                                          data = metadata)
          }else{
            metadata$meanage.financial <- meanage.financial 
            metadata <- metadata %>%
              rename(mean.age = meanage.financial, scale = scale.financial) %>%
              drop_na()
            meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_Estimate,
                                                                            sei = age_Std.Error1,
                                                                            slab = study,
                                                                            mods = ~ continent + mean.age + scale,
                                                                            ni = age_N.obs,
                                                                            data = metadata)
            } 
        print("fixed or lg")
        }
      
      # lg.gender
      if(c==4){
        for(d in 1:length(cofficient_name[[c]])){
          if(d==1){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            metadata <- meta_data_list[[meta_results_name]]
              if(b == 1){
                metadata$meanage.general <- meanage.general 
                metadata <- metadata %>%
                  rename(mean.age = meanage.general, scale = scale.general) %>%
                  drop_na()
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_Estimate,
                                                                                sei = age_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale,
                                                                                ni = age_N.obs,
                                                                                data = metadata)
                }else{
                  metadata$meanage.financial <- meanage.financial 
                  metadata <- metadata %>%
                    rename(mean.age = meanage.financial, scale = scale.financial) %>%
                    drop_na()
                  meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_Estimate,
                                                                                  sei = age_Std.Error1,
                                                                                  slab = study,
                                                                                  mods = ~ continent + mean.age + scale,
                                                                                  ni = age_N.obs,
                                                                                  data = metadata)
                }
            print("a1")
            }
          
          if(d==2){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            metadata <- meta_data_list[[meta_results_name]]
              if(b == 1){
                metadata$meanage.general <- meanage.general 
                metadata <- metadata %>%
                  rename(mean.age = meanage.general, scale = scale.general) %>%
                  drop_na()
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = gender_Estimate,
                                                                                sei = gender_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale,
                                                                                ni = gender_N.obs,
                                                                                data = metadata)
                }else{
                  metadata$meanage.financial <- meanage.financial 
                  metadata <- metadata %>%
                    rename(mean.age = meanage.financial, scale = scale.financial) %>%
                    drop_na()
                  meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = gender_Estimate,
                                                                                  sei = gender_Std.Error1,
                                                                                  slab = study,
                                                                                  mods = ~ continent + mean.age + scale,
                                                                                  ni = gender_N.obs,
                                                                                  data = metadata)
                }
            print("a2")
          }
          }
        print("lg.gender")
        }
      
      
      # lg.inter
      if(c==5){
        for(d in 1:length(cofficient_name[[c]])){
          if(d==1){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            metadata <- meta_data_list[[meta_results_name]]
              if(b == 1){
                metadata$meanage.general <- meanage.general 
                metadata <- metadata %>%
                  rename(mean.age = meanage.general, scale = scale.general) %>%
                  drop_na()
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_Estimate,
                                                                                sei = age_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale,
                                                                                ni = age_N.obs,
                                                                                data = metadata)
                }else{
                  metadata$meanage.financial <- meanage.financial 
                  metadata <- metadata %>%
                    rename(mean.age = meanage.financial, scale = scale.financial) %>%
                    drop_na()
                  meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_Estimate,
                                                                                  sei = age_Std.Error1,
                                                                                  slab = study,
                                                                                  mods = ~ continent + mean.age + scale,
                                                                                  ni = age_N.obs,
                                                                                  data = metadata)
              }
            print("b1")
            }
          
          if(d==2){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            metadata <- meta_data_list[[meta_results_name]]
              if(b == 1){
                metadata$meanage.general <- meanage.general 
                metadata <- metadata %>%
                  rename(mean.age = meanage.general, scale = scale.general) %>%
                  drop_na()
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = gender_Estimate,
                                                                              sei = gender_Std.Error1,
                                                                              slab = study,
                                                                              mods = ~ continent + mean.age + scale,
                                                                              ni = gender_N.obs,
                                                                              data = metadata)
                }else{
                  metadata$meanage.financial <- meanage.financial 
                  metadata <- metadata %>%
                    rename(mean.age = meanage.financial, scale = scale.financial) %>%
                    drop_na()
                  meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = gender_Estimate,
                                                                              sei = gender_Std.Error1,
                                                                              slab = study,
                                                                              mods = ~ continent + mean.age + scale,
                                                                              ni = gender_N.obs,
                                                                              data = metadata)
                }
            print("b2")
            }
          
          if(d==3){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            metadata <- meta_data_list[[meta_results_name]]
              if(b == 1){
                metadata$meanage.general <- meanage.general 
                metadata <- metadata %>%
                  rename(mean.age = meanage.general, scale = scale.general) %>%
                  drop_na()
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_gender_Estimate,
                                                                                sei = age_gender_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale,
                                                                                ni = age_gender_N.obs,
                                                                                data = metadata)
                }else{
                  metadata$meanage.financial <- meanage.financial 
                  metadata <- metadata %>%
                    rename(mean.age = meanage.financial, scale = scale.financial) %>%
                    drop_na()
                  meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_gender_Estimate,
                                                                                  sei = age_gender_Std.Error1,
                                                                                  slab = study,
                                                                                  mods = ~ continent + mean.age + scale,
                                                                                  ni = age_gender_N.obs,
                                                                                  data = metadata)
                }
            print("b3")
          }
          }
        print("lg.inter")
        }
      
      
      # quadratic
      if(c==6){
        for(d in 1:length(cofficient_name[[c]])){
          if(d==1){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            metadata <- meta_data_list[[meta_results_name]]
              if(b == 1){
                metadata$meanage.general <- meanage.general 
                metadata <- metadata %>%
                  rename(mean.age = meanage.general, scale = scale.general) %>%
                  drop_na()
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi= age_Estimate,
                                                                               sei = age_Std.Error1,
                                                                               slab = study,
                                                                               mods = ~ continent + mean.age + scale,
                                                                               ni = age_N.obs,
                                                                               data = metadata)
              }else{
                metadata$meanage.financial <- meanage.financial 
                metadata <- metadata %>%
                  rename(mean.age = meanage.financial, scale = scale.financial) %>%
                  drop_na()
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_Estimate,
                                                                                sei = age_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale,
                                                                                ni = age_N.obs,
                                                                                data = metadata)
              }
            print("c1")
            }
          
          if(d==2){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            metadata <- meta_data_list[[meta_results_name]]
              if(b == 1){
                metadata$meanage.general <- meanage.general 
                metadata <- metadata %>%
                  rename(mean.age = meanage.general, scale = scale.general) %>%
                  drop_na()
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age2_Estimate,
                                                                                sei = age2_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale,
                                                                                ni = age2_N.obs,
                                                                                data = metadata)
              }else{
                metadata$meanage.financial <- meanage.financial 
                metadata <- metadata %>%
                  rename(mean.age = meanage.financial, scale = scale.financial) %>%
                  drop_na()
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age2_Estimate,
                                                                                sei = age2_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale,
                                                                                ni = age2_N.obs,
                                                                                data = metadata)
              }
            print("c2")
          }
          }
        print("quadratic")
      }

      
      # quadratic.gender
      if(c==7){
        for(d in 1:length(cofficient_name[[c]])){
          if (d==1){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            metadata <- meta_data_list[[meta_results_name]]
              if(b == 1){
                metadata$meanage.general <- meanage.general 
                metadata <- metadata %>%
                  rename(mean.age = meanage.general, scale = scale.general) %>%
                  drop_na()
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi= age_Estimate,
                                                                                sei = age_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale,
                                                                                ni = age_N.obs,
                                                                                data = metadata)
              }else{
                metadata$meanage.financial <- meanage.financial 
                metadata <- metadata %>%
                  rename(mean.age = meanage.financial, scale = scale.financial) %>%
                  drop_na()
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_Estimate,
                                                                                sei = age_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale,
                                                                                ni = age_N.obs,
                                                                                data = metadata)
              }
            print("d1")
            }
          
          if(d==2){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            metadata <- meta_data_list[[meta_results_name]]
              if(b == 1){
                metadata$meanage.general <- meanage.general 
                metadata <- metadata %>%
                  rename(mean.age = meanage.general, scale = scale.general) %>%
                  drop_na()
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age2_Estimate,
                                                                                 sei = age2_Std.Error1,
                                                                                 slab = study,
                                                                                 mods = ~ continent + mean.age + scale,
                                                                                 ni = age2_N.obs,
                                                                                 data = metadata)
              }else{
                metadata$meanage.financial <- meanage.financial 
                metadata <- metadata %>%
                  rename(mean.age = meanage.financial, scale = scale.financial) %>%
                  drop_na()
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age2_Estimate,
                                                                                sei = age2_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale,
                                                                                ni = age2_N.obs,
                                                                                data = metadata)
              }
            print("d2")
            }

          if(d==3){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            metadata <- meta_data_list[[meta_results_name]]
              if(b == 1){
                metadata$meanage.general <- meanage.general 
                metadata <- metadata %>%
                  rename(mean.age = meanage.general, scale = scale.general) %>%
                  drop_na()
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = gender_Estimate,
                                                                                sei = gender_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale,
                                                                                ni = gender_N.obs,
                                                                                data = metadata)
              }else{
                metadata$meanage.financial <- meanage.financial 
                metadata <- metadata %>%
                  rename(mean.age = meanage.financial, scale = scale.financial) %>%
                  drop_na()
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = gender_Estimate,
                                                                                sei = gender_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale,
                                                                                ni = gender_N.obs,
                                                                                data = metadata)
              }
            print("d3")
          }
          }
        print("quadratic.gender")
      }
    }
    }
  return(meta_results_list_all)
}

meta_results_list_all <- meta_analysis(ab_domains, models,cofficient_name, meta_data_list, meta_results_list_all)


###save results
saveRDS(meta_results_list_all, file = "results_data/meta_results_list_all.Rds")

### load results ###
#meta_results_list_all <- readRDS(file = "results_data/meta_results_list_all.Rds")
