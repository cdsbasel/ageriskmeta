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
#panels <- c("DHS")
panels <- c("DHS","GCOE_Japan","GCOE_USA","HILDA","HRS","LIKS","PHF","SAVE","SHARE_Austria","SHARE_Belgium","SHARE_Czech_Republic","SHARE_Denmark", "SHARE_Estonia", "SHARE_France", "SHARE_Germany","SHARE_Israel", "SHARE_Italy","SHARE_Netherlands","SHARE_Slovenia","SHARE_Spain","SHARE_Sweden","SHARE_Switzerland","SOEP","USoc")   # do not include Addhealth / NLSY79_Child / NLSY79_YA panel
domains <- c("General", "Financial","Driving", "Recreational", "Occupational", "Health", "Social")
ab_domains <- c("g", "f", "d", "r", "o", "h", "s")
models = c("int", "fixed", "lg", "lg.gender", "lg.inter", "quad", "quad.gender")
model_plot <- models[3:7]


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


### function 2: create empty intdummy_model_list and int icc.se_list ###
create_intdummy_model_list  <- function(ab_domains){
  index=0
  model_name_list <- list()
  dummy_name_list <- list()
  intdummy_model_list <- vector("list", length = length(ab_domains))
  icc.se_list <- vector("list", length = length(ab_domains))
  for (b in 1:length(ab_domains)) {
    index=index+1
    model_name_list[[index]] <- paste0(ab_domains[b],".int")
    dummy_name_list[[index]] <- paste0(ab_domains[b],".dummy")
  }
  names(intdummy_model_list) <- dummy_name_list
  names(icc.se_list) <- model_name_list
  return (list(intdummy_model_list,icc.se_list))
}
# empty_dummy_list <- create_intdummy_model_list(ab_domains)
# intdummy_model_list <- empty_dummy_list[[1]]
# icc.se_list <- empty_dummy_list[[2]]



### function 3:create empty plot_data_list ###
create_plot_data_list <- function(ab_domains,model_plot){
  index=0
  model_name_list <- list()
  plot_data_list <- vector("list", length = length(ab_domains)*length(model_plot))
  for (b in 1:length(ab_domains)) {
    for (c in 1:length(model_plot)) {
    index=index+1
    model_name_list[[index]] <- paste0("plot_",ab_domains[b],".", model_plot[c])
    }
  }
  names(plot_data_list) <- model_name_list         
  return(plot_data_list)
}
# plot_data_list <- create_plot_data_list(ab_domains, model_plot)


### function 4: create 49 models and save them in previous model_list; create 7 intdummy models and save them into precious intdummy_model_list, and save icc.se into icc.se_list###
create_model <- function(panel, models, domains, ab_domains, model_list, intdummy_model_list ,icc.se_list, plot_data_list){
  for(b in 1:length(domains)){
    domain_name <- paste0(domains[b], "_z")
    print(domain_name)
    if(domain_name %in% names(panel)){
      dataset <- panel %>%
        dplyr::select(ID, age, Gender, SYEAR, domain_name) %>%
        dplyr::filter(!is.na(get(domain_name)))  %>%
        dplyr::group_by(ID) %>%
        dplyr::filter(n()>=2) %>%
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
        model_dummy_name <- paste0(ab_domains[b],".dummy")
        print(model_dummy_name)
        model_plot_name <- paste0("plot_",ab_domains[b],".", models[c])
        print(model_plot_name)
        #unconditional means model/intercept-only model/null model
        if(c==1){
          model_list[[model_name]] <- lmer(get(domain_name) ~ (1|ID), data= dataset, REML=F,
                                           control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
          intdummy_model_list[[model_dummy_name]] <- dataset %>% 
            sjstats::bootstrap(100) %>%  #bootstrap:Generates n bootstrap samples of data and returns the bootstrapped data frames as list-variable
            mutate(models = lapply(.$strap, function(x) {
              lmer(get(domain_name) ~ (1|ID), data = x, control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
            })) %>%
            mutate(icc = unlist(lapply(.$models, sjstats::icc))) #use unlist() to convert the list into a vector
            #mutate(icc = lapply(.$models, performance::icc)) %>% 
            #mutate(icc = unlist(sapply(icc, '[', 'ICC_adjusted')))  #just select one of ICC_adjusted or ICC_conditional
          icc.se_list[[model_name]] = sjstats::boot_se(intdummy_model_list[[model_dummy_name]], icc) #boot_se:computes the nonparametric bootstrap standard error by calculating the standard deviation of the input vector.
          print(paste0("icc.se_",model_dummy_name))
        }
        #unconditional growth model with a fixed slope
        if(c==2){
          model_list[[model_name]] <- lmer(get(domain_name) ~ d_c_age + (1|ID), data=dataset, REML=F,
                                           control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
        }
        #unconditional growth model with a fixed/random slope(linear model)
        if(c==3){
          model_list[[model_name]] <- lmer(get(domain_name) ~ d_c_age + (1 + d_c_age|ID), data=dataset, REML=F,
                                           control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
          plot_data_list[[model_plot_name]] <- expand.grid(ID = "999999999",
                                                           d_c_age = seq(from = min(dataset$d_c_age, na.rm = T),
                                                                         to = max(dataset$d_c_age, na.rm=T),
                                                                         length.out = 100))  # expand.grid: Create a data frame from all combinations of the supplied vectors or factors.
          plot_data_list[[model_plot_name]]$fit = predict(model_list[[model_name]], newdata = plot_data_list[[model_plot_name]], allow.new.levels = TRUE) 
          plot_data_list[[model_plot_name]]$age = (plot_data_list[[model_plot_name]]$d_c_age*10) + 50
        }
        #linear growth, with gender
        if(c==4){
          model_list[[model_name]] <- lmer(get(domain_name) ~ d_c_age + gender + (1+d_c_age|ID), data=dataset,REML=F, 
                                           control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
          plot_data_list[[model_plot_name]] = expand.grid(ID = "99999999999",
                                                          d_c_age = seq(from = min(dataset$d_c_age, na.rm = T),
                                                                        to = max(dataset$d_c_age, na.rm=T),
                                                                        length.out = 100),
                                                          gender = unique(dataset$gender))
          plot_data_list[[model_plot_name]]$fit = predict(model_list[[model_name]], newdata = plot_data_list[[model_plot_name]], allow.new.levels = TRUE) 
          plot_data_list[[model_plot_name]]$age = (plot_data_list[[model_plot_name]]$d_c_age*10) + 50
          plot_data_list[[model_plot_name]]$gender = as.factor(plot_data_list[[model_plot_name]]$gender)
        }
        
        #linear growth model, with interaction
        if(c==5){
          model_list[[model_name]] <- lmer(get(domain_name) ~ d_c_age + d_c_age*gender + (1+d_c_age|ID), data=dataset,REML=F, 
                                           control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
          plot_data_list[[model_plot_name]] = expand.grid(ID = "99999999999",
                                                          d_c_age = seq(from = min(dataset$d_c_age, na.rm = T),
                                                                    to = max(dataset$d_c_age, na.rm=T),
                                                                    length.out = 100),
                                                          gender = unique(dataset$gender))
          plot_data_list[[model_plot_name]]$fit = predict(model_list[[model_name]], newdata = plot_data_list[[model_plot_name]], allow.new.levels = TRUE) 
          plot_data_list[[model_plot_name]]$age = (plot_data_list[[model_plot_name]]$d_c_age*10) + 50
          plot_data_list[[model_plot_name]]$gender = as.factor(plot_data_list[[model_plot_name]]$gender)
          
        }
        #quadratic growth model
        if(c==6){
          if(waves >3){
            model_list[[model_name]] <- lmer(get(domain_name) ~ d_c_age + I(d_c_age^2) +(1+d_c_age|ID), data=dataset, REML=F,
                                             control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
            plot_data_list[[model_plot_name]] <- expand.grid(ID = "999999999",
                                                             d_c_age = seq(from = min(dataset$d_c_age, na.rm = T),
                                                                           to = max(dataset$d_c_age, na.rm=T),
                                                                           length.out = 100))  # expand.grid: Create a data frame from all combinations of the supplied vectors or factors.
            plot_data_list[[model_plot_name]]$fit = predict(model_list[[model_name]], newdata = plot_data_list[[model_plot_name]], allow.new.levels = TRUE) 
            plot_data_list[[model_plot_name]]$age = (plot_data_list[[model_plot_name]]$d_c_age*10) + 50
          }
        }
        #quadratic growth model, with gender
        if(c==7){
          if(waves >3){
            model_list[[model_name]] <- lmer(get(domain_name) ~ d_c_age + I(d_c_age^2) + gender + (1+d_c_age|ID), data=dataset,
                                             REML=F, control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
            plot_data_list[[model_plot_name]] = expand.grid(ID = "99999999999",
                                                            d_c_age = seq(from = min(dataset$d_c_age, na.rm = T),
                                                                          to = max(dataset$d_c_age, na.rm=T),
                                                                          length.out = 100),
                                                            gender = unique(dataset$gender))
            plot_data_list[[model_plot_name]]$fit = predict(model_list[[model_name]], newdata = plot_data_list[[model_plot_name]], allow.new.levels = TRUE) 
            plot_data_list[[model_plot_name]]$age = (plot_data_list[[model_plot_name]]$d_c_age*10) + 50
            plot_data_list[[model_plot_name]]$gender = as.factor(plot_data_list[[model_plot_name]]$gender)
          }
        }
        
      }
    }
  }
  return(list(model_list,icc.se_list,intdummy_model_list,plot_data_list))
}
# twotype_model_list <- create_model(panel, models, domains,ab_domains,model_list,intdummy_model_list,icc.se_list,plot_data_list)
# model_list <- twotype_model_list[[1]]
# icc.se_list <- twotype_model_list[[2]]
# intdummy_model_list <- twotype_model_list[[3]]
# plot_data_list <- twotype_model_list[[4]]


### function 5: create empty results list ### 
create_results_list  <- function(models,ab_domains){
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


### function 6: extract parameter from model list and save them into previous results_list ###
extract_model_parameter <- function(ab_domains, models, model_list,icc.se_list,results_list){
  for (b in 1:length(ab_domains)) {
    for (c in 1:length(models)) {
      model_name <- paste0(ab_domains[b],".", models[c])
      results_name <- paste0("results","_",ab_domains[b],".", models[c])
      if(c==1){
        if (!is.null(model_list[[model_name]])){
          results_list[[results_name]] <- list(
            coef = coef(summary(model_list[[model_name]])),
            p = sjstats::p_value(model_list[[model_name]]),
            rand = sjstats::re_var(model_list[[model_name]]),
            icc = sjstats::icc(model_list[[model_name]]),
            icc.se = icc.se_list[[model_name]]$std.err,
            n.group = summary(model_list[[model_name]])[["ngrps"]],
            n.obs = nrow(model_list[[model_name]]@frame),
            ll = summary(model_list[[model_name]])[["logLik"]]
          )}
      }
      if(c %in% 2:7){
        if (!is.null(model_list[[model_name]])){
          results_list[[results_name]] <- list(
            coef = coef(summary(model_list[[model_name]])),
            p = sjstats::p_value(model_list[[model_name]]),
            rand = sjstats::re_var(model_list[[model_name]]),
            n.group = summary(model_list[[model_name]])[["ngrps"]],
            n.obs = nrow(model_list[[model_name]]@frame),
            ll = summary(model_list[[model_name]])[["logLik"]]
          )}
      }
    }
  }
  return(results_list)
}
# results_list <- extract_model_parameter(ab_domains, models, model_list,icc.se_list,results_list)

### create row name list ###
b1 <- c("Intercept_Estimate","Intercept_Std.Error1","Intercept_t.value","Intercept","Intercept_p.value","Intercept_Std.Error2","Within-group-variance","Between-group-variance","icc","icc.se","N.group","N.obs","LL")

b2 <- c("Intercept_Estimate","Intercept_Std.Error1","Intercept_t.value","d_c_age_Estimate","d_c_age_Std.Error1","d_c_age_t.value","Intercept","Intercept_p.value","Intercept_Std.Error2","d_c_age ","d_c_age_p.value","d_c_age_Std.Error2","Within-group-variance","Between-group-variance","N.group","N.obs","LL")

b3 <- c("Intercept_Estimate","Intercept_Std.Error1","Intercept_t.value","d_c_age_Estimate","d_c_age_Std.Error1","d_c_age_t.value","Intercept","Intercept_p.value","Intercept_Std.Error2","d_c_age ","d_c_age_p.value","d_c_age_Std.Error2","Within-group-variance","Between-group-variance","Random-slope-variance","Slope-Intercept-covariance","Slope-Intercept-correlation","N.group","N.obs","LL")

b4 <- c("Intercept_Estimate","Intercept_Std.Error1","Intercept_t.value","d_c_age_Estimate","d_c_age_Std.Error1","d_c_age_t.value","gender_Estimate","gender_Std.Error1","gender_t.value","Intercept","Intercept_p.value","Intercept_Std.Error2","d_c_age ","d_c_age_p.value","d_c_age_Std.Error2","gender","gender_p.value","gender_Std.Error2","Within-group-variance","Between-group-variance","Random-slope-variance","Slope-Intercept-covariance","Slope-Intercept-correlation","N.group","N.obs","LL")

b5 <- c("Intercept_Estimate","Intercept_Std.Error1","Intercept_t.value","d_c_age_Estimate","d_c_age_Std.Error1","d_c_age_t.value","gender_Estimate","gender_Std.Error1","gender_t.value","d_c_age:gender_Estimate","d_c_age:gender_Std.Error1","d_c_age:gender_t.value","Intercept","Intercept_p.value","Intercept_Std.Error2","d_c_age ","d_c_age_p.value","d_c_age_Std.Error2","gender","gender_p.value","gender_Std.Error2","d_c_age:gender","d_c_age:gender_p.value","d_c_age:gender_Std.Error2","Within-group-variance","Between-group-variance","Random-slope-variance","Slope-Intercept-covariance","Slope-Intercept-correlation","N.group","N.obs","LL")

b6 <- c("Intercept_Estimate","Intercept_Std.Error1","Intercept_t.value","d_c_age_Estimate","d_c_age_Std.Error1","d_c_age_t.value","I(d_c_age^2)_Estimate","I(d_c_age^2)_Std.Error1","I(d_c_age^2)_t.value","Intercept","Intercept_p.value","Intercept_Std.Error2","d_c_age ","d_c_age_p.value","d_c_age_Std.Error2","I(d_c_age^2)","I(d_c_age^2)_p.value","I(d_c_age^2)_Std.Error2","Within-group-variance","Between-group-variance","Random-slope-variance","Slope-Intercept-covariance","Slope-Intercept-correlation","N.group","N.obs","LL")

b7 <- c("Intercept_Estimate","Intercept_Std.Error1","Intercept_t.value","d_c_age_Estimate","d_c_age_Std.Error1","d_c_age_t.value","I(d_c_age^2)_Estimate","I(d_c_age^2)_Std.Error1","I(d_c_age^2)_t.value","gender_Estimate","gender_Std.Error1","gender_t.value","Intercept","Intercept_p.value","Intercept_Std.Error2","d_c_age ","d_c_age_p.value","d_c_age_Std.Error2","I(d_c_age^2)","I(d_c_age^2)_p.value","I(d_c_age^2)_Std.Error2","gender","gender_p.value","gender_Std.Error2","Within-group-variance","Between-group-variance","Random-slope-variance","Slope-Intercept-covariance","Slope-Intercept-correlation","N.group","N.obs","LL")

rowname_list <- list(b1,b2,b3,b4,b5,b6,b7)

### function 7: create empty results table list to save model parameters results ### 
create_results_table_list  <- function(models,ab_domains,panels,rowname_list){
  index=0
  results_name_list <- list()  
  results_table_list <- vector("list", length = length(ab_domains)*length(models))
  nrowlist=c(13,17,20,26,32,26,32)
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


### function 8: transform results list into dataframe and save them into previous results_table_list ###
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
# results_table_list <- transform_results(ab_domains, models, results_list, results_table_list,a)


### function 9: create an empty plot_data_results_list to save every panels' model predicted data ###
create_plot_data_results_list  <- function(ab_domains, model_plot, panels){
  index=0
  model_plot_name_list <- list()  
  plot_data_results_list <- vector("list", length = length(ab_domains)*length(model_plot))
  for (b in 1:length(ab_domains)) {
    for (c in 1: length(model_plot)) {
      index=index+1
      plot_data_results_list[[index]] <- vector("list", length = length(panels))
      names(plot_data_results_list[[index]]) <- panels
      model_plot_name_list[[index]] <- paste0("plot_",ab_domains[b],".", model_plot[c])
    }
  }
  names(plot_data_results_list) <- model_plot_name_list
  return(plot_data_results_list)
}
# plot_data_results_list <- create_plot_data_results_list(ab_domains, model_plot, panels)


### function 10: save every panels' model predicted data into previous empty plot_data_results_list###
save_plot_data_list <- function(ab_domains, model_plot,plot_data_list,plot_data_results_list, a){
  for (b in 1:length(ab_domains)) {
    for (c in 1:length(model_plot)) {
      model_plot_name <- paste0("plot_",ab_domains[b],".", model_plot[c])
      if (!is.null(plot_data_list[[model_plot_name]])){
        plot_data_results_list[[model_plot_name]][[a]] <- plot_data_list[[model_plot_name]]
        print(paste0("save_plot_",ab_domains[b],".", model_plot[c]))
      }
    }
  }
  return(plot_data_results_list)
}
# plot_data_results_list <- save_plot_data_list(ab_domains, model_plot,plot_data_list,plot_data_results_list, a)



### function 11: combine previous function together and run them for different panels, export final results, results_table_list ###
model_process = function(panels, domains,  ab_domains, models, model_plot, model_parameter,rowname_list){
  results_table_list <- create_results_table_list(models,ab_domains,panels,rowname_list) # create empty results table list to save model parameters results
  plot_data_results_list <- create_plot_data_results_list(ab_domains, model_plot, panels) # create an empty plot_data_results_list to save every panels' model predicted data
  print("aaaaaa1")
 for(a in 1:length(panels)){
    panel <- readRDS(paste0(panels[a], "_process_z.Rds")) #load all panel data one by one
    print(paste0("panel",a))
    model_list <- create_model_list(ab_domains,models)  #create empty model list
    print("function1")
    empty_dummy_list <- create_intdummy_model_list(ab_domains)
    intdummy_model_list <- empty_dummy_list[[1]]
    icc.se_list <- empty_dummy_list[[2]]
    print("function2")
    plot_data_list <- create_plot_data_list(ab_domains, model_plot)
    print("function3")
    #then, in specific panel data, select the demographic and domain_name column, and do some calculation
    twotype_model_list <- create_model(panel, models, domains,ab_domains,model_list,intdummy_model_list,icc.se_list,plot_data_list)
    model_list <- twotype_model_list[[1]]
    icc.se_list <- twotype_model_list[[2]]
    intdummy_model_list <- twotype_model_list[[3]]
    plot_data_list <- twotype_model_list[[4]]
    print("function4")
    results_list <- create_results_list(models,ab_domains) #create empty model results list
    print("function5")
    results_list <- extract_model_parameter(ab_domains, models, model_list, icc.se_list, results_list) #extract parameter from model list
    print("function6")
    results_table_list <- transform_results(ab_domains, models, results_list, results_table_list, a) #transform results list into dataframe 
    print("function8")
    plot_data_results_list <- save_plot_data_list(ab_domains, model_plot,plot_data_list,plot_data_results_list, a)
    print("function10")
    print("yes,successful")
  }
  return(list(results_table_list,plot_data_results_list))
}

final_results_list <- model_process(panels, domains,  ab_domains, models, model_plot, model_parameter,rowname_list)
results_table_list <- final_results_list[[1]]
plot_data_results_list <- final_results_list[[2]]
### model end

### save results ###
setwd("../data")
saveRDS(results_table_list, file ="results_data/results_table_list.Rds")
saveRDS(plot_data_results_list, file ="results_data/plot_data_results_list.Rds")
  
### load results ###
#results_table_list <- readRDS(file = "results_data/results_table_list.Rds")
#plot_data_results_list <- readRDS(file = "results_data/plot_data_results_list.Rds")


