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

setwd("../data/results_data")

### prepare some loop variable ###
panels = c("DHS", "GCOE_Japan", "GCOE_USA", "HILDA", "HRS", "LIKS", "PHF", "SAVE", 
           "SHARE_Austria", "SHARE_Belgium", "SHARE_Czech_Republic", "SHARE_Denmark", "SHARE_Estonia", "SHARE_France", "SHARE_Germany","SHARE_Israel", "SHARE_Italy", "SHARE_Netherlands", "SHARE_Slovenia", "SHARE_Spain", "SHARE_Sweden", "SHARE_Switzerland", 
           "SOEP","USoc")
domains = c("General", "Financial", "Driving", "Recreational", "Occupational", "Health", "Social")
ab_domains = c("g", "f", "d", "r", "o", "h", "s")
models = c("int", "fixed", "lg", "lg.gender", "lg.inter", "quad", "quad.gender")
model_plot = models[3:7]


### function1: combine exiting data and create a data list
combine_data <- function(ab_domains, model_plot, data_list){
  for (b in 1:length(ab_domains)) {
    for (c in 1: length(model_plot)) {
      plot_name = paste0("plot_",ab_domains[b],".", model_plot[c])
      sub_data = data_list[[plot_name]] 
      sub_data = sub_data[vapply(sub_data, Negate(is.null), NA)]
      if(length(sub_data) > 0) {
        for(i in 1:length(sub_data)){
          name_list = names(sub_data)
          sub_data[[i]]$Panel = name_list[i]
          sub_data[[i]]$Panel = as.factor(sub_data[[i]]$Panel)
        }
        sub_data = Reduce(function(x, y) merge(x, y, all=TRUE), sub_data)
      }
      data_list[[plot_name]] = sub_data
    }
  }
  return(data_list)
}

### load model predict data
plot_model_data_list = readRDS(file = "plot_data_results_list.Rds")

### combine existing data and create_model_data 
plot_model_data_list = combine_data(ab_domains, model_plot, data_list=plot_model_data_list)

### save plot_model_data_list for plot
#saveRDS(plot_model_data_list, file ="plot_model_data_list.Rds")


###function2: create a empty figure_list for saving all plot
c=2
index=0
plot_name_list = list()  
figure_list = vector("list", length = length(ab_domains))
for (b in 1:length(ab_domains)){
  index = index+1
  plot_name_list[[index]] = paste0("plot_",ab_domains[b],".", model_plot[c])
}
names(figure_list) = plot_name_list


index = c("DHS","GCOE_Japan","GCOE_USA","HILDA","HRS","LIKS","PHF","SAVE","SHARE_Austria","SHARE_Belgium","SHARE_Czech_Republic","SHARE_Denmark", "SHARE_Estonia", "SHARE_France", "SHARE_Germany","SHARE_Israel", "SHARE_Italy","SHARE_Netherlands","SHARE_Slovenia","SHARE_Spain","SHARE_Sweden","SHARE_Switzerland","SOEP","USoc")  
values = c("DHS","GCOE","GCOE","HILDA","HRS","LIKS","PHF","SAVE","SHARE","SHARE","SHARE","SHARE", "SHARE", "SHARE", "SHARE","SHARE", "SHARE","SHARE","SHARE","SHARE","SHARE","SHARE","SOEP","USoc") 
lines = c(rep("a", 8), rep("c", 14), rep("a", 2))

colordata = data.frame(matrix(data=NA, nrow=length(panels), ncol = 1)) 
colordata = as.data.frame(matrix(panels))
colnames(colordata) = "Sample"
colordata$Panel = values[match(colordata$Sample, index)]
colordata$Lines = lines[match(colordata$Sample, index)]
colordata$Sample = as.factor(colordata$Sample) 
colordata$Panel = as.factor(colordata$Panel) 
colordata$Lines = as.factor(colordata$Lines) 


# ### plot: plot all panels in one picture (same y scale)
for (b in 1:length(ab_domains)) {
  y_name = paste0(domains[b],"_z_mean")
  y_number = paste0(domains[b],"_z_n")
  y_domain = domains[b]
  plot_name = paste0("plot_",ab_domains[b],".", model_plot[c])

  #model data
  model_data = plot_model_data_list[[plot_name]]
  names(model_data)[names(model_data) == "Panel"] = "Sample"
  model_data$Panel = values[match(model_data$Sample, index)]
  model_data$Lines = lines[match(model_data$Sample, index)]
  
  model_data[,"gender"] = ifelse(model_data$gender == 0, "Male", "Female")
  model_data$gender = factor(model_data$gender, levels = c("Male", "Female"))
  
  
  figure_list[[plot_name]] <- ggplot() + 
    stat_smooth(data = model_data, aes(x = age, y = fit, colour=Sample, linetype=Sample), se=FALSE, method = "lm", formula = y ~ x, size = 0.5) +
    scale_colour_discrete(drop=FALSE, limits = levels(colordata$Sample)) +
    scale_linetype_manual(limits = levels(colordata$Sample), values = c(rep("solid", 8), rep("dashed", 14), rep("solid", 2))) + 
    stat_smooth(data = model_data, aes(x = age, y = fit), color = "black",se=TRUE, method = "lm", formula = y ~ x, size = 1.1) +
    labs(x= "Age", y= paste(y_domain, "risk-taking", sep = " ")) +
    scale_x_continuous(breaks = seq(20, 90, 10))+
    coord_cartesian(ylim = c(-1, 1)) +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA))+
    facet_wrap(~gender, nrow=1, scales="free_x") +
    guides(col = guide_legend(ncol = 1))
}

General = figure_list$plot_g.lg.gender
Financial = figure_list$plot_f.lg.gender
Driving = figure_list$plot_d.lg.gender 
Recreational = figure_list$plot_r.lg.gender
Occupational = figure_list$plot_o.lg.gender
Health = figure_list$plot_h.lg.gender

(General + theme(plot.margin = unit(c(0,20,0,0), "pt"))) /
  (Financial + theme(plot.margin = unit(c(0,20,0,0), "pt")))/
  (Driving + theme(plot.margin = unit(c(10,20,10,0), "pt"))) / 
  (Recreational + theme(plot.margin = unit(c(10,20,10,0), "pt")))/
  (Occupational + theme(plot.margin = unit(c(0,20,0,0), "pt")))/
  (Health + theme(plot.margin = unit(c(0,20,0,0), "pt"))) +
  plot_layout(guides = 'collect', ncol = 2) +
  plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(face = 'bold')) 



