

library(tidyverse)
library(janitor)
source('./old version/table1Setup.R')
flash <- readRDS('flash.data.1003.rds') %>% filter(site != "VU")
reduced <-
  flash |> 
  filter(
    Visit == 1,
    on_bipap == 0,
    VDP_exp > 0
  ) |> 
  mutate(
    best_pre_fev1fvc = if_else(
      site == 'JHU',
      best_pre_fev1fvc * 100,
      best_pre_fev1fvc
    ),
    sgrq = factor(
      case_when(
        sgrq_score <= 9.65 ~ '25%',
        sgrq_score <= 23.81 ~ '50%',
        sgrq_score <= 47.66~ '75%',
        sgrq_score <= 92.30  ~ '100%'
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
    ),
    sgrq_med = if_else(
      sgrq_score < median(sgrq_score, na.rm = TRUE),
      'healthier', 'worse'
    ),
    CAT = factor(
      case_when(
        cat_score <= 6.5  ~ '25%',
        cat_score <= 13.0 ~ '50%',
        cat_score <= 22.0 ~ '75%',
        cat_score <= 38.0  ~ '100%'
      ),
      levels = c('25%', '50%', '75%', '100%'),
      ordered = TRUE
    ),
    CAT2 = factor(
      case_when(
        cat_score <= 8 ~ 'first',
        cat_score <= 16 ~ 'second',
        cat_score <= 24 ~ 'third',
        cat_score <= 32  ~ 'forth',
        cat_score >32  ~ 'fifth',
      ),
      levels = c('first', 'second', 'third', 'forth','fifth'),
      ordered = TRUE
    ),
    cat_med = factor(
      if_else(
        cat_score < median(cat_score, na.rm = TRUE),
        'healthier', 'worse'
      )
    )
  ) 

######################################
# Cat score sqrt score vs MSV VDP VH  FEV1, BMI
###### MSV
reduced |>dplyr::select(sgrq,MSV_exp) %>% drop_na() %>% 
  ggplot(aes(x = sgrq, y = MSV_exp)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'SGRQ category by quantile')

reduced |>dplyr::select(sgrq2,MSV_exp) %>% drop_na() %>% 
  ggplot(aes(x = sgrq2, y = MSV_exp)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'SGRQ category by 20 unit')

reduced |>dplyr::select(CAT,MSV_exp) %>% drop_na() %>% 
  ggplot(aes(x = CAT, y = MSV_exp)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'CAT category by quantile')

reduced |>dplyr::select(CAT2,MSV_exp) %>% drop_na() %>% 
  ggplot(aes(x = CAT2, y = MSV_exp)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'CAT category by 8 unit')

############
###### VH
reduced |>dplyr::select(sgrq,VH_exp) %>% drop_na() %>% 
  ggplot(aes(x = sgrq, y = VH_exp)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'SGRQ category by quantile')

reduced |>dplyr::select(sgrq2,VH_exp) %>% drop_na() %>% 
  ggplot(aes(x = sgrq2, y = VH_exp)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'SGRQ category by 20 unit')

reduced |>dplyr::select(CAT,VH_exp) %>% drop_na() %>% 
  ggplot(aes(x = CAT, y = VH_exp)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'CAT category by quantile')

reduced |>dplyr::select(CAT2,VH_exp) %>% drop_na() %>% 
  ggplot(aes(x = CAT2, y = VH_exp)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'CAT category by 8 unit')
#############FEV1
reduced |>dplyr::select(sgrq,best_pre_fev1) %>% drop_na() %>% 
  ggplot(aes(x = sgrq, y = best_pre_fev1)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'SGRQ category by quantile')

reduced |>dplyr::select(sgrq2,best_pre_fev1) %>% drop_na() %>% 
  ggplot(aes(x = sgrq2, y = best_pre_fev1)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'SGRQ category by 20 unit')

reduced |>dplyr::select(CAT,best_pre_fev1) %>% drop_na() %>% 
  ggplot(aes(x = CAT, y = best_pre_fev1)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'CAT category by quantile')

reduced |>dplyr::select(CAT2,best_pre_fev1) %>% drop_na() %>% 
  ggplot(aes(x = CAT2, y = best_pre_fev1)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'CAT category by 8 unit')

###########BMI
reduced |>dplyr::select(sgrq,bmi_new) %>% drop_na() %>% 
  ggplot(aes(x = sgrq, y = bmi_new)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'SGRQ category by quantile')

reduced |>dplyr::select(sgrq2,bmi_new) %>% drop_na() %>% 
  ggplot(aes(x = sgrq2, y = bmi_new)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'SGRQ category by 20 unit')

reduced |>dplyr::select(CAT,bmi_new) %>% drop_na() %>% 
  ggplot(aes(x = CAT, y = bmi_new)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'CAT category by quantile')

reduced |>dplyr::select(CAT2,bmi_new) %>% drop_na() %>% 
  ggplot(aes(x = CAT2, y = bmi_new)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'CAT category by 8 unit')
