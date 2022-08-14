##### multi-level growth models ######
rm(list=ls())
library(tidyverse) #an opinionated collection of R packages designed for data science
library(lme4)    #Conducts all multilevel models.

require(devtools)
install_version("sjstats", version = "0.17.4", repos = "http://cran.us.r-project.org")  #the version of sjstats must be 0.17.4, else can not run successfully. 
# because icc function in "sjstats 0.17.4" package
library(sjstats) #Collection of convenient functions for common statistical computations
# packageVersion("sjstats")

### set working directory
setwd("../data/process_z_data")

### prepare some loop variable ###
panels <- c("DHS","GCOE_Japan","GCOE_USA","GLES","HILDA","HRS","LIKS","PHF","SAVE","SHARE_Austria","SHARE_Belgium","SHARE_Czech_Republic","SHARE_Denmark", "SHARE_Estonia", "SHARE_France", "SHARE_Germany","SHARE_Israel", "SHARE_Italy","SHARE_Netherlands","SHARE_Slovenia","SHARE_Spain","SHARE_Sweden","SHARE_Switzerland","SOEP","UAS","USoc")   # do not include Addhealth / NLSY79_Child / NLSY79_YA panel
domains <- c("General", "Financial","Driving", "Recreational", "Occupational", "Health", "Social")
ab_domains <- c("g", "f", "d", "r", "o", "h", "s")
models = c("lg.gender.group", "lg.gender.group.inter")


### function 1: create empty model list ###
create_model_list  <- function(ab_domains,models){
  index=0
  model_name_list <- list()
  model_list <- vector("list", length = length(ab_domains)*length(models))
  for (b in 1:length(ab_domains)) {
    for (c in 1:length(models)) {
      index=index+1
      model_name_list[[index]] = paste0(ab_domains[b],".", models[c])
    }
  }
  names(model_list) <- model_name_list
  return(model_list)
}
# model_list <- create_model_list(ab_domains,models)


### function 2: create empty plot_data_list ###
create_plot_data_list <- function(ab_domains,models){
  index=0
  model_name_list <- list()
  plot_data_list <- vector("list", length = length(ab_domains)*length(models))
  for (b in 1:length(ab_domains)) {
    for (c in 1:length(models)) {
    index=index+1
    model_name_list[[index]] <- paste0("plot_",ab_domains[b],".", models[c])
    }
  }
  names(plot_data_list) <- model_name_list         
  return(plot_data_list)
}
# plot_data_list <- create_plot_data_list(ab_domains, models)


### function 3: create 14 models and save them in previous model_list;
create_model <- function(panel, models, domains, ab_domains, model_list, plot_data_list){
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
        mutate(age.group = if_else(age <= 60, 0, 1)) %>%
        mutate(gender = if_else(Gender == 2, 1, 0)) 
      waves <- length(unique(dataset$SYEAR))
      
      # create models
      for (c in 1:length(models)) {
        model_name <- paste0(ab_domains[b],".", models[c])
        print(model_name)
        
        model_plot_name <- paste0("plot_",ab_domains[b],".", models[c])
        print(model_plot_name)
        
        # lg.gender.group model
        if(c==1){
          model_list[[model_name]] <- lmer(get(domain_name) ~ d.c.age + gender + age.group + (1+d.c.age|ID), data=dataset,REML=F, 
                                           control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
          plot_data_list[[model_plot_name]] = expand.grid(ID = "99999999999",
                                                          d.c.age = seq(from = min(dataset$d.c.age, na.rm = T),
                                                                        to = max(dataset$d.c.age, na.rm=T),
                                                                        length.out = 100),
                                                          gender = unique(dataset$gender),
                                                          age.group = unique(dataset$age.group))
          plot_data_list[[model_plot_name]]$fit = predict(model_list[[model_name]], newdata = plot_data_list[[model_plot_name]], allow.new.levels = TRUE) 
          plot_data_list[[model_plot_name]]$age = (plot_data_list[[model_plot_name]]$d.c.age*10) + 50
          plot_data_list[[model_plot_name]]$gender = as.factor(plot_data_list[[model_plot_name]]$gender)
          plot_data_list[[model_plot_name]]$age.group = as.factor(plot_data_list[[model_plot_name]]$age.group)
        }
        
        #lg.gender.group.inter model
        if(c==2){
          model_list[[model_name]] <- lmer(get(domain_name) ~ d.c.age + gender + age.group + d.c.age * gender * age.group + (1+d.c.age|ID), data=dataset,REML=F, 
                                           control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
          plot_data_list[[model_plot_name]] = expand.grid(ID = "99999999999",
                                                          d.c.age = seq(from = min(dataset$d.c.age, na.rm = T),
                                                                    to = max(dataset$d.c.age, na.rm=T),
                                                                    length.out = 100),
                                                          gender = unique(dataset$gender),
                                                          age.group = unique(dataset$age.group))
          plot_data_list[[model_plot_name]]$fit = predict(model_list[[model_name]], newdata = plot_data_list[[model_plot_name]], allow.new.levels = TRUE) 
          plot_data_list[[model_plot_name]]$age = (plot_data_list[[model_plot_name]]$d.c.age*10) + 50
          plot_data_list[[model_plot_name]]$gender = as.factor(plot_data_list[[model_plot_name]]$gender)
          plot_data_list[[model_plot_name]]$age.group = as.factor(plot_data_list[[model_plot_name]]$age.group)
        }
      }
    }
  }
  return(list(model_list, plot_data_list))
}
# twotype_model_list <- create_model(panel, models, domains,ab_domains,model_list, plot_data_list)
# model_list <- twotype_model_list[[1]]
# plot_data_list <- twotype_model_list[[2]]


### function 4: create empty results list ### 
create_results_list  <- function(models, ab_domains){
  index=0
  results_name_list <- list()  
  results_list <- vector("list", length = length(ab_domains)*length(models))
  for (b in 1:length(ab_domains)) {
    for (c in 1: length(models)) {
      index=index+1
      results_name_list[[index]] = paste0("results","_",ab_domains[b],".", models[c])
    }
  }
    names(results_list) <- results_name_list
    return(results_list)
}
# results_list <- create_results_list(models,ab_domains)


### function 5: extract parameter from model list and save them into previous results_list ###
extract_model_parameter <- function(ab_domains, models, model_list, results_list){
  for (b in 1:length(ab_domains)) {
    for (c in 1:length(models)) {
      model_name <- paste0(ab_domains[b],".", models[c])
      results_name <- paste0("results","_",ab_domains[b],".", models[c])
      
      if (!is.null(model_list[[model_name]])){
        results_list[[results_name]] <- list(
          coef = coef(summary(model_list[[model_name]])),
          p = sjstats::p_value(model_list[[model_name]]),
          rand = sjstats::re_var(model_list[[model_name]]),
          n.group = summary(model_list[[model_name]])[["ngrps"]],
          n.obs = nrow(model_list[[model_name]]@frame),
          ll = summary(model_list[[model_name]])[["logLik"]])
      }
    }
  }
  return(results_list)
}
# results_list <- extract_model_parameter(ab_domains, models, model_list, results_list)

### create row name list ###
b1 <- c("Intercept_Estimate","Intercept_Std.Error1","Intercept_t.value", "d.c.age_Estimate","d.c.age_Std.Error1","d.c.age_t.value", "gender_Estimate","gender_Std.Error1","gender_t.value", "age.group_Estimate","age.group_Std.Error1","age.group_t.value",
        "Intercept","Intercept_p.value","Intercept_Std.Error2", "d.c.age","d.c.age_p.value","d.c.age_Std.Error2", "gender","gender_p.value","gender_Std.Error2", "age.group","age.group_p.value","age.group_Std.Error2",
        "Within-group-variance","Between-group-variance","Random-slope-variance","Slope-Intercept-covariance","Slope-Intercept-correlation","N.group","N.obs","LL")

b2 <- c("Intercept_Estimate","Intercept_Std.Error1","Intercept_t.value", "d.c.age_Estimate","d.c.age_Std.Error1","d.c.age_t.value", "gender_Estimate","gender_Std.Error1","gender_t.value", "age.group_Estimate","age.group_Std.Error1","age.group_t.value", "d.c.age:gender_Estimate","d.c.age:gender_Std.Error1","d.c.age:gender_t.value", "d.c.age:age.group_Estimate","d.c.age:age.group_Std.Error1","d.c.age:age.group_t.value", "gender:age.group_Estimate", "gender:age.group_Std.Error1","gender:age.group_t.value", "d.c.age:gender:age.group_Estimate", "d.c.age:gender:age.group_Std.Error1","d.c.age:gender:age.group_t.value",
        "Intercept","Intercept_p.value","Intercept_Std.Error2", "d.c.age ","d.c.age_p.value","d.c.age_Std.Error2", "gender","gender_p.value","gender_Std.Error2", "age.group","age.group_p.value","age.group_Std.Error2", "d.c.age:gender","d.c.age:gender_p.value","d.c.age:gender_Std.Error2", "d.c.age:age.group","d.c.age:age.group_p.value","d.c.age:age.group_Std.Error2", "gender:age.group","gender:age.group_p.value","gender:age.group_Std.Error2", "d.c.age:gender:age.group","d.c.age:gender:age.group_p.value","d.c.age:gender:age.group_Std.Error2",
        "Within-group-variance","Between-group-variance","Random-slope-variance","Slope-Intercept-covariance","Slope-Intercept-correlation","N.group","N.obs","LL")

rowname_list <- list(b1,b2)

### function 6: create empty results table list to save model parameters results ### 
create_results_table_list  <- function(models, ab_domains, panels, rowname_list){
  index=0
  results_name_list <- list()  
  results_table_list <- vector("list", length = length(ab_domains)*length(models))
  nrowlist=c(32, 56)
  for (b in 1:length(ab_domains)) {
    for (c in 1: length(models)) {
      index=index+1
      results_table_list[[index]] <- data.frame(matrix(data=NA, nrow=nrowlist[c], ncol = length(panels)))
      colnames(results_table_list[[index]]) <- panels
      rownames(results_table_list[[index]]) <- rowname_list[[c]]
      results_name_list[[index]] <- paste0("results","_",ab_domains[b],".", models[c])
    }
  }
  names(results_table_list) <- results_name_list
  return(results_table_list)
}
# results_table_list <- create_results_table_list(models,ab_domains,panels,rowname_list)


### function 7: transform results list into dataframe and save them into previous results_table_list ###
transform_results <- function(ab_domains, models, results_list, results_table_list, a){
  for (b in 1:length(ab_domains)) {
    for (c in 1:length(models)) {
      results_model_name <- paste0("results","_",ab_domains[b],".", models[c])
      if (!is.null(results_list[[results_model_name]])){
        for (d in 1:length(results_list[[results_model_name]])){
        results_list[[results_model_name]][[d]] <- t(results_list[[results_model_name]][[d]])
        }
        results_table_list[[results_model_name]][a] <- as.data.frame(matrix(as.numeric(unlist(results_list[[results_model_name]]))))
      }
    }
  }
  return(results_table_list)
}
# results_table_list <- transform_results(ab_domains, models, results_list, results_table_list, a)


### function 8: create an empty plot_data_results_list to save every panels' model predicted data ###
create_plot_data_results_list  <- function(ab_domains, models, panels){
  index=0
  model_plot_name_list <- list()  
  plot_data_results_list <- vector("list", length = length(ab_domains)*length(models))
  for (b in 1:length(ab_domains)) {
    for (c in 1: length(models)) {
      index=index+1
      plot_data_results_list[[index]] <- vector("list", length = length(panels))
      names(plot_data_results_list[[index]]) <- panels
      model_plot_name_list[[index]] <- paste0("plot_",ab_domains[b],".", models[c])
    }
  }
  names(plot_data_results_list) <- model_plot_name_list
  return(plot_data_results_list)
}
# plot_data_results_list <- create_plot_data_results_list(ab_domains, models, panels)


### function 9: save every panels' model predicted data into previous empty plot_data_results_list###
save_plot_data_list <- function(ab_domains, models,plot_data_list,plot_data_results_list, a){
  for (b in 1:length(ab_domains)) {
    for (c in 1:length(models)) {
      model_plot_name <- paste0("plot_",ab_domains[b],".", models[c])
      if (!is.null(plot_data_list[[model_plot_name]])){
        plot_data_results_list[[model_plot_name]][[a]] <- plot_data_list[[model_plot_name]]
        print(paste0("save_plot_",ab_domains[b],".", models[c]))
      }
    }
  }
  return(plot_data_results_list)
}
# plot_data_results_list <- save_plot_data_list(ab_domains, models, plot_data_list, plot_data_results_list, a)



### combine previous function together and run them for different panels, export final results, results_table_list ###
model_process = function(panels, domains,  ab_domains, models, model_parameter,rowname_list){
  results_table_list <- create_results_table_list(models,ab_domains,panels,rowname_list) # function 6: create empty results table list to save model parameters results
  plot_data_results_list <- create_plot_data_results_list(ab_domains, models, panels) #function 8: create an empty plot_data_results_list to save every panels' model predicted data
  print("aaaaaa1")
 for(a in 1:length(panels)){
    panel <- readRDS(paste0(panels[a], "_process_z.Rds")) #load all panel data one by one
    print(paste0("panel",a))
    model_list <- create_model_list(ab_domains,models)  #create empty model list
    print("function1")
    plot_data_list <- create_plot_data_list(ab_domains, models) #create empty plot_data_list
    print("function2")
    #then, in specific panel data, select the demographic and domain_name column, and run the model
    twotype_model_list <- create_model(panel, models, domains,ab_domains, model_list, plot_data_list)
    model_list <- twotype_model_list[[1]]
    plot_data_list <- twotype_model_list[[2]]
    print("function3")
    results_list <- create_results_list(models,ab_domains) #create empty model results list
    print("function4")
    results_list <- extract_model_parameter(ab_domains, models, model_list, results_list) #extract parameter from model list
    print("function5")
    results_table_list <- transform_results(ab_domains, models, results_list, results_table_list, a) #transform results list into dataframe 
    print("function7")
    plot_data_results_list <- save_plot_data_list(ab_domains, models, plot_data_list,plot_data_results_list, a)
    print("function9")
    print("yes,successful")
  }
  return(list(results_table_list,plot_data_results_list))
}

final_results_list_cohort <- model_process(panels, domains,  ab_domains, models, model_parameter,rowname_list)
results_table_list_cohort <- final_results_list_cohort[[1]]
plot_data_results_list_cohort <- final_results_list_cohort[[2]]
### model end

### save results ###
setwd(".../data")
saveRDS(results_table_list_cohort, file ="results_data/results_table_list_cohort.Rds")
saveRDS(plot_data_results_list_cohort, file ="results_data/plot_data_results_list_cohort.Rds")
  
### load results ###
#results_table_list_cohort <- readRDS(file = "results_data/results_table_list_cohort.Rds")
#plot_data_results_list_cohort <- readRDS(file = "results_data/plot_data_results_list_cohort.Rds")


