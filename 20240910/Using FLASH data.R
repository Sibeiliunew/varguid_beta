source("./leash2.0.6.R")
source("./VarGuid20240626.R")
library(glmnet)
library(tidyverse)
library(MASS)
library(xtable)
library(scales)
library(faux)
library(olsrr)
library(caTools)

flash <- read.csv("FLAS_REDCap_data_03-27-2024.csv")
lvd <- read.csv("resultLVD.csv")

flash <- merge(lvd,flash,by.x = "study_id", by.y="record_id")


dat=data.frame(scale(flash[,c("M.DFM","V.DFM","M.PRR","V.PRR","M.PRN","V.PRN")]),
                      age=flash$age,
                      gender=flash$gender-1,
                      smoke=ifelse(is.na(flash$smokingpacks), 0, 1),
                      bmi=flash$bmi,
                      sgrq_score=flash$total_sgrq_score,
                      cat_score=flash$cat_score
) %>% janitor::clean_names()

####################
flash <- readRDS("~/Documents/Dissertation/varguid/flash.data.1003.rds")

# dat=data.frame(scale(flash[,c("M.DFM","V.DFM","M.PRR","V.PRR","M.PRN","V.PRN")]),
#                age=flash$age,
#                gender=flash$gender-1,
#                smoke=ifelse(is.na(flash$smokingpacks), 0, 1),
#                bmi=flash$bmi,
#                sgrq_score=flash$total_sgrq_score,
#                cat_score=flash$cat_score
# ) %>% janitor::clean_names()

dat=flash[,c(5,6,19:95,111:118,124)] %>% mutate(gender=as.numeric(factor(gender,levels=c("male","female")))-1)

dat[,3:87]=apply(dat[,3:87],2,scale)

#################
table_fun=function(X,Y){
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

res1=as.data.frame(table_fun(X=dat,Y=flash$sgrq_score))
t1=cbind(name=colnames(dat),round(res1,4))
write_csv(t1,"sgrq.csv")

res2=as.data.frame(table_fun(X=dat,Y=flash$cat_score))
t2=cbind(name=colnames(dat),round(res2,4))
write_csv(t2,"cat.csv")

###################################
#################
m2<- lm(sgrq_score~ m_prn+v_prn+age+gender+smoke+bmi, data = dat)
ols_test_score(m2)
ols_test_f(m2)
ols_test_breusch_pagan(m2)


summary(m2)
dat1=dat%>% drop_na()

m2.1<- lmv(X=dat1[,c(5,6,7:10)],Y=dat1[,11]) #  sgrq
summary(m2.1$obj.varGuid)
rm(m2,dat1,m2.1)
#######################################################
#######################################################
#################
m2<- lm(cat_score~ m_prn+v_prn+age+gender+smoke+bmi, data = dat)
ols_test_score(m2)
ols_test_f(m2)
ols_test_breusch_pagan(m2)


summary(m2)
dat1=dat%>% drop_na()

m2.1<- lmv(X=dat1[,c(5,6,7:10)],Y=dat1[,12])#  cat
summary(m2.1$obj.varGuid)
rm(m2,dat1,m2.1)
