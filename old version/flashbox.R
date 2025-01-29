library(tidyverse)
library(janitor)
flash <- readRDS("flash.rds")

###figure 1:
flash %>% select(sgrq2,MSV_exp) %>% drop_na() %>% 
  ggplot(aes(x = sgrq2, y = MSV_exp)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'SGRQ category by 20 unit')

####figure2
flash %>% select(CAT2,best_pre_fev1) %>% drop_na() %>% 
  ggplot(aes(x = CAT2, y = best_pre_fev1)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'CAT category by 8 unit')


###### square residual vs predicted 