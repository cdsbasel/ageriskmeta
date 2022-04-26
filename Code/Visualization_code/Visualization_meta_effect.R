### Visualization meta_effect###
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(data.table)

###set working directory
setwd("../data/results_data")

###prepare some items
panels = c("DHS", "GCOE_Japan", "GCOE_USA", "HILDA", "HRS", "LIKS", "PHF", "SAVE", 
           "SHARE_Austria", "SHARE_Belgium", "SHARE_Czech_Republic", "SHARE_Denmark", "SHARE_Estonia", "SHARE_France", "SHARE_Germany","SHARE_Israel", "SHARE_Italy", "SHARE_Netherlands", "SHARE_Slovenia", "SHARE_Spain", "SHARE_Sweden", "SHARE_Switzerland", 
           "SOEP","USoc")
domains = c("General", "Financial", "Driving", "Recreational", "Occupational", "Health", "Social")
ab_domains = c("g", "f", "d", "r", "o", "h", "s")
models = c("int", "fixed", "lg", "lg.gender", "lg.inter", "quad", "quad.gender")
model_plot = models[3:7]

cofficient_name = list(list("icc"),  # int
                       list("age"),  # fixed
                       list("age"),  # lg
                       list("age", "gender"),  # lg.gender
                       list("age", "gender", "age_gender"), # lg.inter
                       list("age","age2"),  # quad
                       list("age", "age2", "gender"))  # quad.gender

parameters = c("b","SE","Z","p","CI.lb","CI.ub")
raw_parameters = c("beta", "se", "zval", "pval", "ci.lb", "ci.ub")

###load meta results data
meta_results_list = readRDS(file = "meta_results_list_no_mods.Rds")

###create a empty list(meta_summary_list) to save meta analysis parameters
creat_meta_summary_list <- function(ab_domains, models,cofficient_name, parameters,domains){
  index=0
  name_list = list()
  meta_summary_list = vector("list", length = length(ab_domains)*length(models))
  for (b in 1:length(ab_domains)) {
    for (c in 1: length(models)) {
      index=index+1
      name_list[[index]] = paste0("meta", "_", ab_domains[b], ".", models[c])
      meta_summary_list[[index]] = vector("list", length = length(cofficient_name[[c]]))
      for (d in 1:length(cofficient_name[[c]])){
        meta_summary_list[[index]][[d]] = data.frame(matrix(data=NA, nrow = 1, ncol=6))
        names(meta_summary_list[[index]]) = unlist(cofficient_name[[c]])
        colnames(meta_summary_list[[index]][[d]]) = parameters
        rownames(meta_summary_list[[index]][[d]]) = paste0(domains[b])
      }
    }
  }
  names(meta_summary_list) = name_list
  return(meta_summary_list)
}
meta_summary_list = creat_meta_summary_list(ab_domains, models,cofficient_name, parameters, domains)


###select necessary parameters from meta_results_list and save into meta_summary_list
select_summary_parameters <- function(ab_domains, models, cofficient_name, parameters, raw_parameters, meta_summary_list,meta_results_list){
  index = 0
  for (b in 1:length(ab_domains)) {
    for (c in 1:length(models)) {
      meta_name = paste0("meta", "_", ab_domains[b], ".", models[c])
      for (d in 1:length(cofficient_name[[c]])){
        cofficient = cofficient_name[[c]][[d]]
        if (!is.null(meta_results_list[[meta_name]])){
          for (e in 1:length(parameters)) {
            summary_colnames = parameters[e]
            results_colnames = raw_parameters[e]
            meta_summary_list[[meta_name]][[cofficient]][[summary_colnames]] = meta_results_list[[meta_name]][[cofficient]][[results_colnames]]
          }
        }
      }
    }
  }
  return(meta_summary_list)
}
meta_summary_list = select_summary_parameters(ab_domains, models, cofficient_name, parameters, raw_parameters, meta_summary_list, meta_results_list)


###create a empty combine_meta_summary_list to save combined domain meta summary(meta_summary_list)
create_combine_meta_summary_list <- function(models, cofficient_name){
  index = 0
  summary_name_list = list()  
  combine_meta_summary_list = vector("list", length = length(models))
  for (c in 1: length(models)) {
    index = index+1
    summary_name_list[[index]] = paste0("meta", "_", models[c])
    combine_meta_summary_list[[index]] = vector("list", length = length(cofficient_name[[c]]))
    for (d in 1:length(cofficient_name[[c]])){
      names(combine_meta_summary_list[[index]]) = unlist(cofficient_name[[c]])
    }
  }
  names(combine_meta_summary_list) = summary_name_list
  return(combine_meta_summary_list)
}
combine_meta_summary_list = create_combine_meta_summary_list(models, cofficient_name)


###combine domain meta summary(meta_summary_list) and save into combine_meta_summary_list
combine_meta_summary <- function(models, ab_domains, cofficient_name, combine_meta_summary_list, meta_summary_list){
  for (c in 1: length(models)) {
    meta_model = paste0("meta", "_", models[c])
    name_list = c()
    index=0
    for (b in 1:length(ab_domains)){
      index = index+1
      name_list[index] = paste0("meta", "_", ab_domains[b], ".", models[c]) 
      }
    for (d in 1:length(cofficient_name[[c]])){
      cofficient = cofficient_name[[c]][[d]]
      combine_meta_summary_list[[meta_model]][[cofficient]] <- do.call("rbind", list(meta_summary_list[[(name_list[1])]][[cofficient]],
                                                                                     meta_summary_list[[(name_list[2])]][[cofficient]],
                                                                                     meta_summary_list[[(name_list[3])]][[cofficient]],
                                                                                     meta_summary_list[[(name_list[4])]][[cofficient]],
                                                                                     meta_summary_list[[(name_list[5])]][[cofficient]],
                                                                                     meta_summary_list[[(name_list[6])]][[cofficient]],
                                                                                     meta_summary_list[[(name_list[7])]][[cofficient]]))
      combine_meta_summary_list[[meta_model]][[cofficient]] = tibble::rownames_to_column(combine_meta_summary_list[[meta_model]][[cofficient]], "Domain")
    }
  }
  return(combine_meta_summary_list)
}
combine_meta_summary_list = combine_meta_summary(models, ab_domains, cofficient_name, combine_meta_summary_list, meta_summary_list)

### save combine_meta_summary_list for plot
saveRDS(combine_meta_summary_list, file ="combine_meta_summary_list.Rds")


domains_new = c("General", "Financial","Driving", "Recreational", "Occupational", "Health")
colordata = data.frame(matrix(data=NA, nrow=length(domains_new), ncol = 1)) 
colordata = as.data.frame(matrix(domains_new))
colnames(colordata) = "domains_name"
colordata$domains_name = as.factor(colordata$domains_name) 


###plot
### Intercept-only model
plot_data = combine_meta_summary_list$meta_int$icc
plot_data$Domain = factor(plot_data$Domain, levels = c("General", "Financial","Driving", "Recreational", "Occupational", "Health"))
plot_data$Domain = fct_rev(plot_data$Domain)

ggplot(data=plot_data, aes(x = Domain, y = b)) + 
  geom_point(aes(group=Domain, colour=Domain), size  = 3) +
  scale_colour_discrete(drop=FALSE,limits = levels(colordata$domains_name)) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub, group=Domain, colour=Domain), width = 0.4, size  = 0.7) +
  labs(x="Domain", y= "ICC") + 
  coord_flip(ylim = c(0,0.6)) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=9.5), strip.text = element_text(size = 9.5),axis.title.x = element_text(size = 10, face="bold"),axis.title.y = element_text(size = 10, face="bold"), plot.title = element_text(size=12, hjust = 0, vjust = 2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA))


### linear growth, with gender (same x scale)
plot_data1 = combine_meta_summary_list$meta_lg.gender$age
plot_data1$Domain = factor(plot_data1$Domain, levels = c("General", "Financial","Driving", "Recreational", "Occupational", "Health"))
plot_data1$Domain = fct_rev(plot_data1$Domain)

plot_data2 = combine_meta_summary_list$meta_lg.gender$gender
plot_data2$Domain = factor(plot_data2$Domain, levels = c("General", "Financial","Driving", "Recreational", "Occupational", "Health"))
plot_data2$Domain = fct_rev(plot_data2$Domain)

meta_lg.gender_age <- ggplot(data=plot_data1, aes(x = Domain, y = b)) + 
  geom_point(aes(group=Domain, colour=Domain), size  = 3) +
  scale_colour_discrete(drop=FALSE,limits = levels(colordata$domains_name)) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub, group=Domain, colour=Domain), width = 0.4, size  = 0.7) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.6) +
  labs(y="Age coefficient") +
  coord_flip(ylim = c(-0.5,0.05)) +
  theme_minimal() +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.border = element_rect(colour = "black", fill=NA)) 


meta_lg.gender_gender <- ggplot(data=plot_data2, aes(x = Domain, y = b)) + 
  geom_point(aes(group=Domain, colour=Domain), size  = 3) +
  scale_colour_discrete(drop=FALSE,limits = levels(colordata$domains_name)) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub, group=Domain, colour=Domain), width = 0.4, size  = 0.7) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.6) +
  labs(x="Domain", y= "Gender coefficient",) +
  coord_flip(ylim = c(-0.5,0.05)) +
  theme_minimal() +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.border = element_rect(colour = "black", fill=NA)) 


(meta_lg.gender_age + theme(plot.margin = unit(c(0,40,0,0), "pt"))) /(meta_lg.gender_gender+theme(plot.margin = unit(c(0,30,0,0), "pt")))  +
  plot_layout(guides = 'collect',ncol = 2) +
  plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(face = 'bold')) 





