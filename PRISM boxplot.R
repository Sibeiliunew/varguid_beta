source("./leash2.0.7.R")
source("./VarGuid20240626.R")
library(glmnet)
library(tidyverse)
library(MASS)
library(xtable)
library(scales)
library(faux)
library(olsrr)
library(caTools)

prism <- readRDS("prism.rds") %>% filter(lung_spiro != 'Missing') %>%  
  dplyr::select(id,age,sex,smokenow,biomass_current,bmi_median,education,
                diagtb , diagchd ,diagdm,sgrq_score) %>% mutate(
                  sex=factor(sex),
                  sex=as.numeric(sex)-1,
                  smokenow=as.numeric(smokenow)-1,
                  biomass_current=as.numeric(biomass_current)-1,
                  education=as.numeric(education)-1, 
                  diagdm=as.numeric(diagdm)-1,
                  diagtb=as.numeric(diagtb)-1, 
                  diagchd=as.numeric(diagchd)-1
                )



prism2=prism %>% mutate( sgrq1 = factor(
  case_when(
    sgrq_score <= quantile(prism$sgrq_score,na.rm = T)[2] ~ '25%',
    sgrq_score <= quantile(prism$sgrq_score,na.rm = T)[3] ~ '50%',
    sgrq_score <= quantile(prism$sgrq_score,na.rm = T)[4] ~ '75%',
    sgrq_score <= quantile(prism$sgrq_score,na.rm = T)[5] ~ '100%'
  ),
  levels = c('25%', '50%', '75%', '100%'),
  ordered = TRUE
),
sgrq2 = factor(
  case_when(
    sgrq_score <= 20~ 'first',
    sgrq_score <= 40 ~ 'second',
    sgrq_score <= 60~ 'third',
    sgrq_score <= 80  ~ 'forth',
    sgrq_score > 80  ~ 'fifth'
  ),
  levels = c('first', 'second', 'third', 'forth','fifth'),
  ordered = TRUE
))


prism2 %>% dplyr::select(sgrq1,bmi_median) %>% drop_na() %>% 
  ggplot(aes(x = sgrq1, y = bmi_median)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'SGRQ category by quantile')

prism2 %>% dplyr::select(sgrq2,bmi_median) %>% drop_na() %>% 
  ggplot(aes(x = sgrq2, y = bmi_median)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'SGRQ category by 20 unit')
