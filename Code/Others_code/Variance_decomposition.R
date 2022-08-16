### Variance Decomposition ###
rm(list=ls())
### Load package
library(tidyverse)
library(specr)
library(lme4)

### set working directory
setwd(".../data/results_data/")


### Load data
re_parameters <- read_csv("meta_regression_parameter.csv")


### Decompose Variance for Age Effect
# Estimate model
lmer_Age <- lmer(B_Age ~  1 +  (1|Domain) + (1|Continent) + (1|Scale) + (1|Survey_year)  + (1|Sample),  
                 data = re_parameters, control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2E6)))

# Check model summary
summary(lmer_Age)


#Estimate intraclass correlation coefficients
icc_specs(lmer_Age) %>%
  mutate_if(is.numeric, round, 2)


#Plot variance components
plot_variance(lmer_Age) +
  ylim(0, 100)




### Decompose Variance for Gender Effect
# Estimate model
lmer_Gender <- lmer(B_Gender ~  1 +  (1|Domain) + (1|Continent) + (1|Scale) + (1|Survey_year) + (1|Sample),  
                    data = re_parameters, control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2E6)))


# Check model summary
summary(lmer_Gender)


#Estimate intraclass correlation coefficients
icc_specs(lmer_Gender) %>%
  mutate_if(is.numeric, round, 2)


#Plot variance components
plot_variance(lmer_Gender) +
  ylim(0, 100)

