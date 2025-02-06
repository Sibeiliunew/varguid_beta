source("./leash2.0.6.R")
source("./VarGuid20250205.R")
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
         diagtb , diagchd ,diagdm,sgrq_total_score) %>% mutate(
           sex=factor(sex),
           sex=as.numeric(sex)-1,
           smokenow=as.numeric(smokenow)-1,
           biomass_current=as.numeric(biomass_current)-1,
           education=as.numeric(education)-1, 
           diagdm=as.numeric(diagdm)-1,
           diagtb=as.numeric(diagtb)-1, 
           diagchd=as.numeric(diagchd)-1
         )


table_fun2=function(X,Y){
  table=NULL
  for(i in 1:ncol(X)){
    X1=X[,i]
    m2<- lm(Y~unlist(X1))
    w1=ols_test_score(m2)
    w2=ols_test_f(m2)
    w3=ols_test_breusch_pagan(m2)
    
    c1=summary(m2)$coefficients[-1,-3]
    dat1=data.frame(X=X1,Y=Y)%>% drop_na()
    df1=list(X2=dat1[,1:(ncol(dat1)-1)],Y=dat1$Y)
    
    m2.1<- lmv(X=df1$X2,Y=df1$Y)
    c2=summary(m2.1$obj.varGuid)$coefficients[-1,-3]
    res1=c(w1$score,w1$p,w2$f,w2$p,w3$bp,w3$p,c1,c2)
    table=rbind(table,res1)
  }
  colnames(table)=c("score_sta","score_p","f_sta","f_p","bp_sta",'bp_p',"Est","SE","P","Est","SE","P")
  return(table)
}

tb1=table_fun2(X=prism[,2:10],Y=prism$sgrq_total_score )
tb1=round(tb1,5)



  
   df=prism[,2:11]
    m2<- lm(sgrq_total_score~.,data=df)
    w1=ols_test_score(m2)
    w2=ols_test_f(m2)
    w3=ols_test_breusch_pagan(m2)
    
    c1=summary(m2)$coefficients[-1,-3]
    dat1=df%>% drop_na()
    df1=list(X2=dat1[,1:(ncol(dat1)-1)],Y=dat1$sgrq_total_score)
    
    m2.1<- lmv(X=df1$X2,Y=df1$Y)
    c2=summary(m2.1$obj.varGuid)$coefficients[-1,-3]
    list(round(c(w1$score,w1$p,w2$f,w2$p,w3$bp,w3$p),5))
    colnames(c1)=c("Est","SE","P")
    colnames(c2)=c("Est","SE","P")
t=round(rbind(c1,c2),5)

#############. the grouped plot bw bmi and sgrq

prism2=prism %>% mutate( bmi1 = factor(
  case_when(
    sgrq_total_score <= quantile(prism$sgrq_total_score)[2] ~ '25%',
    sgrq_total_score <= quantile(prism$sgrq_total_score)[3] ~ '50%',
    sgrq_total_score <= quantile(prism$sgrq_total_score)[4] ~ '75%',
    sgrq_total_score <= quantile(prism$sgrq_total_score)[5] ~ '100%'
  ),
  levels = c('25%', '50%', '75%', '100%'),
  ordered = TRUE
),
bmi2 = factor(
  case_when(
    sgrq_total_score <= 20~ 'first',
    sgrq_total_score <= 40 ~ 'second',
    sgrq_total_score <= 60~ 'third',
    sgrq_total_score <= 80  ~ 'forth',
    sgrq_total_score > 80  ~ 'fifth'
  ),
  levels = c('first', 'second', 'third', 'forth','fifth'),
  ordered = TRUE
))


prism2 |>dplyr::select(sgrq,bmi_median) %>% drop_na() %>% 
  ggplot(aes(x = sgrq, y = bmi_median)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'SGRQ category by quantile')

prism2 |>dplyr::select(sgrq2,bmi_median) %>% drop_na() %>% 
  ggplot(aes(x = sgrq2, y = bmi_median)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'SGRQ category by 20 unit')





