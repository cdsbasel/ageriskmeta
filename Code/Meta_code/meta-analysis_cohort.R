##### meta analysis for all parameters ######
rm(list=ls())
library(metafor)
library(tidyverse)

setwd("../data")
results_table_list_cohort <- readRDS(file = "results_data/results_table_list_cohort.Rds")

### prepare some loop variable ###
panels <- c("DHS","GCOE_Japan","GCOE_USA","GLES","HILDA","HRS","LIKS","PHF","SAVE","SHARE_Austria","SHARE_Belgium","SHARE_Czech_Republic","SHARE_Denmark", "SHARE_Estonia", "SHARE_France", "SHARE_Germany","SHARE_Israel", "SHARE_Italy","SHARE_Netherlands","SHARE_Slovenia","SHARE_Spain","SHARE_Sweden","SHARE_Switzerland","SOEP","UAS","USoc")   # do not include Addhealth / NLSY79_Child / NLSY79_YA panel
domains <- c("General", "Financial","Driving", "Recreational", "Occupational", "Health", "Social")
ab_domains <- c("g", "f", "d", "r", "o", "h", "s")
models = c("lg.gender.group", "lg.gender.group.inter")

cofficients1 <- list(list("age_Estimate", "age_Std.Error1", "age_N.obs",   "gender_Estimate", "gender_Std.Error1", "gender_N.obs", "age.group_Estimate","age.group_Std.Error1","age.group_N.obs"),  # lg.gender.group model
                     list("age_Estimate", "age_Std.Error1", "age_N.obs",   "gender_Estimate", "gender_Std.Error1", "gender_N.obs",  "age.group_Estimate","age.group_Std.Error1","age.group_N.obs",  "age_gender_Estimate", "age_gender_Std.Error1", "age_gender_N.obs", "age_age.group_Estimate", "age_age.group_Std.Error1", "age_age.group_N.obs", "gender_age.group_Estimate", "gender_age.group_Std.Error1","gender_age.group_N.obs", "age_gender_age.group_Estimate", "age_gender_age.group_Std.Error1","age_gender_age.group_N.obs"))  # lg.gender.group.inter model

cofficients2 <- list(list("d.c.age_Estimate", "d.c.age_Std.Error1", "N.obs",  "gender_Estimate", "gender_Std.Error1", "N.obs", "age.group_Estimate","age.group_Std.Error1","N.obs"), # lg.gender.group model
                     list("d.c.age_Estimate", "d.c.age_Std.Error1", "N.obs",  "gender_Estimate", "gender_Std.Error1", "N.obs", "age.group_Estimate","age.group_Std.Error1","N.obs", "d.c.age:gender_Estimate","d.c.age:gender_Std.Error1","N.obs", "d.c.age:age.group_Estimate","d.c.age:age.group_Std.Error1","N.obs", "gender:age.group_Estimate","gender:age.group_Std.Error1","N.obs", "d.c.age:gender:age.group_Estimate","d.c.age:gender:age.group_Std.Error1","N.obs"))  # lg.gender.group.inter model


cofficient_name <- list(list("age", "gender", "age.group"),  # lg.gender.group model
                        list("age", "gender", "age.group", "age_gender", "age_age.group", "gender_age.group", "age_gender_age.group")) # lg.gender.group.inter model
                    
 

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


###function 2: extract meta analysis needed parameter from results_table_list_cohort and save them into meta_data_list
extract_meta_parameter <- function(ab_domains, models, coefficients1, cofficients2, meta_data_list, results_table_list_cohort){
  for (b in 1:length(ab_domains)) {
    for (c in 1:length(models)) {
      meta_model_name <- paste0("meta","_",ab_domains[b],".", models[c])
      results_model_name <- paste0("results","_",ab_domains[b],".", models[c])
        for(d in 1:length(cofficients1[[c]])){
          results_rowname <- cofficients2[[c]][[d]]
          meta_rowname <- cofficients1[[c]][[d]]
        meta_data_list[[meta_model_name]][meta_rowname,] <- results_table_list_cohort[[results_model_name]][results_rowname,]
        }
        meta_data_list[[meta_model_name]] <- as.data.frame(t(meta_data_list[[meta_model_name]]))
        meta_data_list[[meta_model_name]] <- tibble::rownames_to_column(meta_data_list[[meta_model_name]], "study")
    }
  }
  return(meta_data_list)
}
meta_data_list <- extract_meta_parameter(ab_domains, models, coefficients1, cofficients2, meta_data_list, results_table_list_cohort)

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
meta_results_list <- create_meta_results_list(ab_domains, models, cofficient_name)


### function 4: do meta analysis and save the results into previous meta_results_list
# slab = study  note: slab means study labels, optional vector with labels for the studies.
meta_analysis <- function(ab_domains, models,cofficient_name, meta_data_list, meta_results_list) {
  for (b in 1:length(ab_domains)) {
    for (c in 1: length(models)) {
      meta_results_name <- paste0("meta","_",ab_domains[b],".", models[c])
      print(paste0("b=", b))
      print(paste0("c=", c))
      

      # lg.gender.group
      if(c==1){
        metadata <- meta_data_list[[meta_results_name]] %>%
          drop_na()
        for(d in 1:length(cofficient_name[[c]])){
          if(d==1){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
            meta_results_list[[meta_results_name]][[cofficient]] <- rma(yi = age_Estimate,
                                                                        sei = age_Std.Error1,
                                                                        slab = study,
                                                                        ni = age_N.obs,
                                                                        data = metadata)
            }
            print("a1")
          }
          
          if(d==2){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
            meta_results_list[[meta_results_name]][[cofficient]] <- rma(yi = gender_Estimate,
                                                                        sei = gender_Std.Error1,
                                                                        slab = study,
                                                                        ni = gender_N.obs,
                                                                        data = metadata)
            }
            print("a2")
          }
          
          if(d==3){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
              meta_results_list[[meta_results_name]][[cofficient]] <- rma(yi = age.group_Estimate,
                                                                          sei = age.group_Std.Error1,
                                                                          slab = study,
                                                                          ni = age.group_N.obs,
                                                                          data = metadata)
          }
            print("a3")
        }
        print("lg.gender.group")
        }
      }
      
        
      # "lg.gender.group.inter"
      if(c==2){
        metadata <- meta_data_list[[meta_results_name]] %>%
          drop_na()
        for(d in 1:length(cofficient_name[[c]])){
          if(d==1){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
            meta_results_list[[meta_results_name]][[cofficient]] <- rma(yi = age_Estimate,
                                                                        sei = age_Std.Error1,
                                                                        slab = study,
                                                                        ni = age_N.obs,
                                                                        data = metadata)
            }
            print("b1")
          }
          
          if(d==2){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
            meta_results_list[[meta_results_name]][[cofficient]] <- rma(yi = gender_Estimate,
                                                                        sei = gender_Std.Error1,
                                                                        slab = study,
                                                                        ni = gender_N.obs,
                                                                        data = metadata)
            }
            print("b2")
          }
          
          if(d==3){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
              meta_results_list[[meta_results_name]][[cofficient]] <- rma(yi = age.group_Estimate,
                                                                          sei = age.group_Std.Error1,
                                                                          slab = study,
                                                                          ni = age.group_N.obs,
                                                                          data = metadata)
            }
            print("b3")
          }
          
          
          if(d==4){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
            meta_results_list[[meta_results_name]][[cofficient]] <- rma(yi = age_gender_Estimate,
                                                                        sei = age_gender_Std.Error1,
                                                                        ni = age_gender_N.obs,
                                                                        data = metadata)
            }
            print("b4")
          }
          
          if(d==5){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
              meta_results_list[[meta_results_name]][[cofficient]] <- rma(yi = age_age.group_Estimate,
                                                                          sei = age_age.group_Std.Error1,
                                                                          ni = age_age.group_N.obs,
                                                                          data = metadata)
            }
            print("b5")
          }
          
          
          if(d==6){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
              meta_results_list[[meta_results_name]][[cofficient]] <- rma(yi = gender_age.group_Estimate,
                                                                          sei = gender_age.group_Std.Error1,
                                                                          ni = gender_age.group_N.obs,
                                                                          data = metadata)
            }
            print("b6")
          }
          
          if(d==7){
            cofficient <- cofficient_name[[c]][[d]]
            print(cofficient)
            if(nrow(metadata) > 1){
              meta_results_list[[meta_results_name]][[cofficient]] <- rma(yi = age_gender_age.group_Estimate,
                                                                          sei = age_gender_age.group_Std.Error1,
                                                                          ni = age_gender_age.group_N.obs,
                                                                          data = metadata)
            }
            print("b7")
          }
        }
        print("lg.gender.group.inter")
        }
    }
  }
  return(meta_results_list)
}

meta_results_list_cohort <- meta_analysis(ab_domains, models,cofficient_name, meta_data_list, meta_results_list)



###save results
saveRDS(meta_results_list_cohort, file = "results_data/meta_results_list_cohort.Rds")


### load results ###
#meta_results_list_cohort <- readRDS(file = "results_data/meta_results_list_cohort.Rds")

