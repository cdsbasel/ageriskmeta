### Visualization ###
rm(list=ls())
library(metafor)
library(lme4)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(grid)
library(RColorBrewer)
library(patchwork)
devtools::install_github("zeehio/facetscales")
library(facetscales)

setwd(".../data")

### prepare some loop variable ###
panels <- c("DHS","GCOE_Japan","GCOE_USA","GLES","HILDA","HRS","LIKS","PHF","SAVE","SHARE_Austria","SHARE_Belgium","SHARE_Czech_Republic","SHARE_Denmark", "SHARE_Estonia", "SHARE_France", "SHARE_Germany","SHARE_Israel", "SHARE_Italy","SHARE_Netherlands","SHARE_Slovenia","SHARE_Spain","SHARE_Sweden","SHARE_Switzerland","SOEP","UAS","USoc")   # do not include Addhealth / NLSY79_Child / NLSY79_YA panel
domains <- c("General", "Financial","Driving", "Recreational", "Occupational", "Health", "Social")
ab_domains <- c("g", "f", "d", "r", "o", "h", "s")
models = c("lg.gender.group", "lg.gender.group.inter")

### function1: combine exiting data and create a data list
combine_data <-function(ab_domains, model_plot, data_list){
  for (b in 1:length(ab_domains)) {
    for (c in 1: length(model_plot)) {
      plot_name <- paste0("plot_",ab_domains[b],".", model_plot[c])
      print(plot_name)
      sub_data <- data_list[[plot_name]] 
      sub_data <- sub_data[vapply(sub_data, Negate(is.null), NA)]
      if(length(sub_data) > 0) {
        for(i in 1:length(sub_data)){
          name_list <- names(sub_data)
          sub_data[[i]]$Panel <- name_list[i]
          sub_data[[i]]$Panel <- as.factor(sub_data[[i]]$Panel)
        }
        sub_data <- Reduce(function(x, y) merge(x, y, all=TRUE), sub_data)
      }
      data_list[[plot_name]] <- sub_data
    }
  }
  return(data_list)
}

### load model predict data
plot_model_data_list_cohort <- readRDS(file = "results_data/plot_data_results_list_cohort.Rds") 

### combine existing data and create_model_data 
plot_model_data_list_cohort <- combine_data(ab_domains, models, data_list=plot_model_data_list_cohort)

### save plot_model_data_list for plot
#saveRDS(plot_model_data_list_cohort, file ="results_data/plot_model_data_list_cohort.Rds")

#plot_model_data_list_cohort <- readRDS(file = "results_data/plot_model_data_list_cohort.Rds") 

###function2: create a empty figure_list for saving all plot
index=0
plot_name_list <- list()  
figure_list <- vector("list", length = length(ab_domains)*length(models))
for (b in 1:length(ab_domains)){
  for (c in 1:length(models)){
  index=index+1
  plot_name_list[[index]] <- paste0("plot_",ab_domains[b],".", models[c])
  }
}
names(figure_list) <- plot_name_list


index <- c("DHS","GCOE_Japan","GCOE_USA","GLES","HILDA","HRS","LIKS","PHF","SAVE","SHARE_Austria","SHARE_Belgium","SHARE_Czech_Republic","SHARE_Denmark", "SHARE_Estonia", "SHARE_France", "SHARE_Germany","SHARE_Israel", "SHARE_Italy","SHARE_Netherlands","SHARE_Slovenia","SHARE_Spain","SHARE_Sweden","SHARE_Switzerland","SOEP","UAS","USoc")  
values <- c("DHS","GCOE_Japan","GCOE_USA","GLES","HILDA","HRS","LIKS","PHF","SAVE","SHARE_Austria","SHARE_Belgium","SHARE_Czech_Republic","SHARE_Denmark", "SHARE_Estonia", "SHARE_France", "SHARE_Germany","SHARE_Israel", "SHARE_Italy","SHARE_Netherlands","SHARE_Slovenia","SHARE_Spain","SHARE_Sweden","SHARE_Switzerland","SOEP","UAS","USoc")  
lines <- c(rep("a", 9), rep("c", 14), rep("a", 3))

colordata <- data.frame(matrix(data=NA, nrow=length(panels), ncol = 1)) 
colordata <- as.data.frame(matrix(panels))
colnames(colordata) <-"Sample"
colordata$Panel <- values[match(colordata$Sample, index)]
colordata$Lines <- lines[match(colordata$Sample, index)]
colordata$Sample<- as.factor(colordata$Sample) 
colordata$Panel<- as.factor(colordata$Panel) 
colordata$Lines <- as.factor(colordata$Lines) 

scales_x <- list(
  "Under 60 years old" = scale_x_continuous(limits = c(15, 60),breaks =seq(20, 60, 10)),
  "Above 60 years old" = scale_x_continuous(limits = c(60, 105),breaks =seq(60, 100, 10)))

#### plot: plot all panels in one picture (same y scale)

###lg.gender.group model
c=1
for (b in 1:length(ab_domains)) {
  y_name <- paste0(domains[b],"_z_mean")
  y_number <- paste0(domains[b],"_z_n")
  y_domain <- domains[b]
  print(y_name)
  print(y_number)
  print(y_domain)
  
  plot_name <- paste0("plot_",ab_domains[b],".", models[c])
  print(plot_name)
  
  #model data
  model_data <- plot_model_data_list_cohort[[plot_name]] %>%
    filter(!(age.group == 0 & age > 60)) %>%
    filter(!(age.group == 1 & age <= 60)) 
    
  names(model_data)[names(model_data) == "Panel"] <- "Sample"
  model_data$Panel <- values[match(model_data$Sample, index)]
  model_data$Lines <- lines[match(model_data$Sample, index)]
  
  model_data[,"gender"] <- ifelse(model_data$gender == 0, "Male", "Female")
  model_data$gender = factor(model_data$gender, levels = c("Male", "Female"))
  
  model_data[,"age.group"] <- ifelse(model_data$age.group == 0, "Under 60 years old", "Above 60 years old")
  model_data$age.group = factor(model_data$age.group, levels = c("Under 60 years old", "Above 60 years old"))
  
  
  figure_list[[plot_name]] <- ggplot() + 
    stat_smooth(data = model_data, aes(x = age, y = fit, colour=Sample), se=FALSE, method = "lm", formula = y ~ x, size = 0.5) +
    #stat_smooth(data = model_data, aes(x = age, y = fit, colour=Sample, linetype=Sample), se=FALSE, method = "lm", formula = y ~ x, size = 0.5) +
    #scale_colour_discrete(drop=FALSE, limits = levels(colordata$Sample)) +
    #scale_linetype_manual(limits = levels(colordata$Sample), values = c(rep("solid", 9), rep("dashed", 14), rep("solid", 3))) + 
    stat_smooth(data = model_data, aes(x = age, y = fit), color = "black", se=TRUE, method = "lm", formula = y ~ x, size = 1.1) +
    labs(x= "Age", y= paste(y_domain,"risk-taking",sep = " ")) +
    #labs(title=y_domain, x= "Age", y= "Risk propensity") +
    #scale_x_continuous(scales_x)+
    coord_cartesian(ylim = c(-1, 1)) +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA))+
    facet_grid_sc(rows = vars(gender), cols = vars(age.group), scales = list(x = scales_x)) +
    guides(col = guide_legend(ncol = 1))
}

General <- figure_list$plot_g.lg.gender.group
Financial <-figure_list$plot_f.lg.gender.group
Driving <- figure_list$plot_d.lg.gender.group
Recreational <- figure_list$plot_r.lg.gender.group
Occupational <- figure_list$plot_o.lg.gender.group
Health <- figure_list$plot_h.lg.gender.group




###lg.gender.group.inter model
c=2
for (b in 1:length(ab_domains)) {
  y_name <- paste0(domains[b],"_z_mean")
  y_number <- paste0(domains[b],"_z_n")
  y_domain <- domains[b]
  print(y_name)
  print(y_number)
  print(y_domain)
  
  plot_name <- paste0("plot_",ab_domains[b],".", models[c])
  print(plot_name)
  
  #model data
  model_data <- plot_model_data_list_cohort[[plot_name]] %>%
    filter(!(age.group == 0 & age > 60)) %>%
    filter(!(age.group == 1 & age <= 60)) 
  
  names(model_data)[names(model_data) == "Panel"] <- "Sample"
  model_data$Panel <- values[match(model_data$Sample, index)]
  model_data$Lines <- lines[match(model_data$Sample, index)]
  
  model_data[,"gender"] <- ifelse(model_data$gender == 0, "Male", "Female")
  model_data$gender = factor(model_data$gender, levels = c("Male", "Female"))
  
  model_data[,"age.group"] <- ifelse(model_data$age.group == 0, "Under 60 years old", "Above 60 years old")
  model_data$age.group = factor(model_data$age.group, levels = c("Under 60 years old", "Above 60 years old"))
  
  
  figure_list[[plot_name]] <- ggplot() + 
    stat_smooth(data = model_data, aes(x = age, y = fit, colour=Sample), se=FALSE, method = "lm", formula = y ~ x, size = 0.5) +
    #stat_smooth(data = model_data, aes(x = age, y = fit, colour=Sample, linetype=Sample), se=FALSE, method = "lm", formula = y ~ x, size = 0.5) +
    #scale_colour_discrete(drop=FALSE, limits = levels(colordata$Sample)) +
    #scale_linetype_manual(limits = levels(colordata$Sample), values = c(rep("solid", 9), rep("dashed", 14), rep("solid", 3))) + 
    stat_smooth(data = model_data, aes(x = age, y = fit), color = "black", se=TRUE, method = "lm", formula = y ~ x, size = 1.1) +
    labs(x= "Age", y= paste(y_domain,"risk-taking",sep = " ")) +
    #labs(title=y_domain, x= "Age", y= "Risk propensity") +
    #scale_x_continuous(scales_x)+
    coord_cartesian(ylim = c(-1, 1)) +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA))+
    facet_grid_sc(rows = vars(gender), cols = vars(age.group), scales = list(x = scales_x)) +
    guides(col = guide_legend(ncol = 1))
}

General <- figure_list$plot_g.lg.gender.group.inter
Financial <-figure_list$plot_f.lg.gender.group.inter
Driving <- figure_list$plot_d.lg.gender.group.inter
Recreational <- figure_list$plot_r.lg.gender.group.inter
Occupational <- figure_list$plot_o.lg.gender.group.inter
Health <- figure_list$plot_h.lg.gender.group.inter


