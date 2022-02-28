##### meta analysis for all parameters ######
rm(list=ls())
library(metafor)
library(tidyverse)

setwd("../data")
results_table_list <- readRDS(file = "results_data/results_table_list.Rds")

### prepare some loop variable ###
panels <- c("DHS","GCOE_Japan","GCOE_USA","HILDA","HRS","LIKS","PHF","SAVE","SHARE_Austria","SHARE_Belgium","SHARE_Czech_Republic","SHARE_Denmark", "SHARE_Estonia", "SHARE_France", "SHARE_Germany","SHARE_Israel", "SHARE_Italy","SHARE_Netherlands","SHARE_Slovenia","SHARE_Spain","SHARE_Sweden","SHARE_Switzerland","SOEP","USoc")   # do not include Addhealth / NLSY79_Child / NLSY79_YA panel
domains <- c("General", "Financial")   #just do meta-analysis with all moderator for general and financial domain, since other domains only include 3 panels
ab_domains <- c("g", "f")
models = c("int", "fixed", "lg", "lg.gender", "lg.inter", "quad", "quad.gender")

research <- c(1, rep(2, 2), 3, 4, 5, 6, 7,
              rep(8, 14),
              9, 10)

country = c("Netherlands","Japan","USA","Australia","USA","Kyrgyzstan","Germany","Germany","Austria","Belgium","Czech Republic","Denmark","Estonia","France","Germany","Israel","Italy","Netherlands","Slovenia","Spain","Sweden","Switzerland","Germany","UK")
continent = c("Europe","Asia","North America","Oceania","North America","Asia","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Asia","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe")

mean.age = c(52.28, 50.69, 50.24, 50.08, 67.32, 41.26, 55.00, 52.85, 
             65.60, 64.34, 65.43, 62.73, 66.85, 65.40, 65.56, 68.30, 65.56, 64.37, 65.90, 66.15, 67.05, 64.85,
             48.95, 51.47)

scale1 = c(7, 11, 11, 4, 11, 11, 11, 7,
           rep(4, 14), 
           11, 11)

scale2 = c(7, 11, 11, 4, 11, 11, 4, 7,
           rep(4, 14), 
           11, 11)


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
        meta_data_list[[meta_model_name]]$mean.age <- mean.age
        meta_data_list[[meta_model_name]]$scale1 <- scale1
        meta_data_list[[meta_model_name]]$scale2 <- scale2
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
meta_analysis <- function(ab_domains, models,cofficient_name, meta_data_list, meta_results_list, meta_results_list_continent, meta_results_list_mean.age, meta_results_list_scale) {
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
        metadata <- meta_data_list[[meta_results_name]] %>%
          drop_na()
        if(nrow(metadata) > 1){
          if(b == 1){
            meta_results_list_all[[meta_results_name]][[cofficient]] <- rma (yi = icc,
                                                                              sei = icc.se,
                                                                              slab = study,
                                                                              mods = ~ continent + mean.age + scale1,
                                                                              ni = icc_N.obs,
                                                                              data = metadata)
            }else{
              meta_results_list_all[[meta_results_name]][[cofficient]] <- rma (yi = icc,
                                                                                sei = icc.se,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale2,
                                                                                ni = icc_N.obs,
                                                                                data = metadata)
            }
          }
        print("icc")
      }
      
      
      # fixed or lg
      if(c %in% 2:3){
        d = length(cofficient_name[[c]])  #d=1
        cofficient <- cofficient_name[[c]][[d]]
        print(cofficient)
        metadata <- meta_data_list[[meta_results_name]] %>%
          drop_na()
        if(nrow(metadata) > 1){
          if(b == 1){
            meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_Estimate,
                                                                            sei = age_Std.Error1,
                                                                            slab = study,
                                                                            mods = ~ continent + mean.age + scale1,
                                                                            ni = age_N.obs,
                                                                            data = metadata)
            }else{
              meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_Estimate,
                                                                              sei = age_Std.Error1,
                                                                              slab = study,
                                                                              mods = ~ continent + mean.age + scale2,
                                                                              ni = age_N.obs,
                                                                              data = metadata)
            }
          }
        print("fixed or lg")
        }
      
      
      # lg.gender
      if(c==4){
        metadata <- meta_data_list[[meta_results_name]] %>%
          drop_na()
        for(d in 1:length(cofficient_name[[c]])){
          
          if(d==1){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
              if(b == 1){
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_Estimate,
                                                                                sei = age_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale1,
                                                                                ni = age_N.obs,
                                                                                data = metadata)
                }else{
                  meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_Estimate,
                                                                                  sei = age_Std.Error1,
                                                                                  slab = study,
                                                                                  mods = ~ continent + mean.age + scale2,
                                                                                  ni = age_N.obs,
                                                                                  data = metadata)
                }
              }
            print("a1")
            }
          
          if(d==2){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
              if(b == 1){
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = gender_Estimate,
                                                                                sei = gender_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale1,
                                                                                ni = gender_N.obs,
                                                                                data = metadata)
                }else{
                  meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = gender_Estimate,
                                                                                  sei = gender_Std.Error1,
                                                                                  slab = study,
                                                                                  mods = ~ continent + mean.age + scale2,
                                                                                  ni = gender_N.obs,
                                                                                  data = metadata)
                }
              }
            print("a2")
          }
          }
        print("lg.gender")
        }
      
      
      # lg.inter
      if(c==5){
        metadata <- meta_data_list[[meta_results_name]] %>%
          drop_na()
        for(d in 1:length(cofficient_name[[c]])){
          if(d==1){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
              if(b == 1){
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_Estimate,
                                                                                sei = age_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale1,
                                                                                ni = age_N.obs,
                                                                                data = metadata)
                }else{
                  meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_Estimate,
                                                                                  sei = age_Std.Error1,
                                                                                  slab = study,
                                                                                  mods = ~ continent + mean.age + scale2,
                                                                                  ni = age_N.obs,
                                                                                  data = metadata)
                }
              }
            print("b1")
            }
          
          if(d==2){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
              if(b == 1){
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = gender_Estimate,
                                                                              sei = gender_Std.Error1,
                                                                              slab = study,
                                                                              mods = ~ continent + mean.age + scale1,
                                                                              ni = gender_N.obs,
                                                                              data = metadata)
                }else{
                  meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = gender_Estimate,
                                                                              sei = gender_Std.Error1,
                                                                              slab = study,
                                                                              mods = ~ continent + mean.age + scale2,
                                                                              ni = gender_N.obs,
                                                                              data = metadata)
                }
              }
            print("b2")
            }
          
          if(d==3){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
              if(b == 1){
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_gender_Estimate,
                                                                                sei = age_gender_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale1,
                                                                                ni = age_gender_N.obs,
                                                                                data = metadata)
                }else{
                  meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_gender_Estimate,
                                                                                  sei = age_gender_Std.Error1,
                                                                                  slab = study,
                                                                                  mods = ~ continent + mean.age + scale2,
                                                                                  ni = age_gender_N.obs,
                                                                                  data = metadata)
                }
              }
            print("b3")
          }
          }
        print("lg.inter")
        }
      
      
      # quadratic
      if(c==6){
        metadata <- meta_data_list[[meta_results_name]] %>%
          drop_na()
        for(d in 1:length(cofficient_name[[c]])){
          if(d==1){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
              if(b == 1){
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi= age_Estimate,
                                                                               sei = age_Std.Error1,
                                                                               slab = study,
                                                                               mods = ~ continent + mean.age + scale1,
                                                                               ni = age_N.obs,
                                                                               data = metadata)
              }else{
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_Estimate,
                                                                                sei = age_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale2,
                                                                                ni = age_N.obs,
                                                                                data = metadata)
              }
              }
            print("c1")
            }
          
          if(d==2){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
              if(b == 1){
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age2_Estimate,
                                                                                sei = age2_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale1,
                                                                                ni = age2_N.obs,
                                                                                data = metadata)
              }else{
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age2_Estimate,
                                                                                sei = age2_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale2,
                                                                                ni = age2_N.obs,
                                                                                data = metadata)
              }
              }
            print("c2")
          }
          }
        print("quadratic")
      }

      
      # quadratic.gender
      if(c==7){
        metadata <- meta_data_list[[meta_results_name]] %>%
          drop_na()
        for(d in 1:length(cofficient_name[[c]])){
          if (d==1){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
              if(b == 1){
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi= age_Estimate,
                                                                                sei = age_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale1,
                                                                                ni = age_N.obs,
                                                                                data = metadata)
              }else{
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age_Estimate,
                                                                                sei = age_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale2,
                                                                                ni = age_N.obs,
                                                                                data = metadata)
              }
              }
            print("d1")
            }
          
          if(d==2){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
              if(b == 1){
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age2_Estimate,
                                                                                 sei = age2_Std.Error1,
                                                                                 slab = study,
                                                                                 mods = ~ continent + mean.age + scale1,
                                                                                 ni = age2_N.obs,
                                                                                 data = metadata)
              }else{
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = age2_Estimate,
                                                                                sei = age2_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale2,
                                                                                ni = age2_N.obs,
                                                                                data = metadata)
              }
              }
            print("d2")
            }

          if(d==3){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
              if(b == 1){
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = gender_Estimate,
                                                                                sei = gender_Std.Error1,
                                                                                slab = study,
                                                                                mods =~ continent + mean.age + scale1,
                                                                                ni = gender_N.obs,
                                                                                data = metadata)
              }else{
                meta_results_list_all[[meta_results_name]][[cofficient]] <- rma(yi = gender_Estimate,
                                                                                sei = gender_Std.Error1,
                                                                                slab = study,
                                                                                mods = ~ continent + mean.age + scale2,
                                                                                ni = gender_N.obs,
                                                                                data = metadata)
              }
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

meta_results_list_all <- meta_analysis(ab_domains, models,cofficient_name, meta_data_list, meta_results_list, meta_results_list_continent, meta_results_list_mean.age, meta_results_list_scale)


###save results
saveRDS(meta_results_list_all, file = "results_data/meta_results_list_all.Rds")


### load results ###
#meta_results_list_all <- readRDS(file = "results_data/meta_results_list_all.Rds")
