source("./leash2.0.8.R")
source("./VarGuid20250209.R")
source('./table simulation functions.R')
library(glmnet)
library(tidyverse)
library(MASS)
library(xtable)
library(scales)
library(faux)
library(olsrr)

##########################
##########
########## below simulated data were saved inro RDS file
###########################



nsim <- 1000

#######################################################
##########
##########scenario2
#########################################################

## n=20 ,p=10, cor=0

sce2_1=sim_varguid(n=20,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=c(rep(1,5),rep(0,5)),corrv=0,name="sce2") 

apply(sce2_1$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce2_1$heter_test_sta,2,mean ),3)

## n=20 ,p=10, cor=0.9
sce2_2=sim_varguid(n=20,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=c(rep(1,5),rep(0,5)),corrv=0.9,name="sce2") 

apply(sce2_2$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce2_2$heter_test_sta,2,mean ),3)
### n=200, p=10, cov=0
sce2_3=sim_varguid(n=200,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=c(rep(1,5),rep(0,5)),corrv=0,name="sce2") 

apply(sce2_3$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce2_3$heter_test_sta,2,mean ),3)
### n=200, p=10, cov=0.9
sce2_4=sim_varguid(n=200,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=c(rep(1,5),rep(0,5)),corrv=0.9,name="sce2") 

apply(sce2_4$heter_test_p,2,function(x)length(which(x<0.05))/nsim )
round(apply(sce2_4$heter_test_sta,2,mean ),3)
#######################################################
##########
##########scenario4
#########################################################

sce4_1=sim_varguid(n=20,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=c(rep(0,5),rep(1,5)),corrv=0,name="sce4") 



apply(sce4_1$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce4_1$heter_test_sta,2,mean ),3)
#######
sce4_2=sim_varguid(n=20,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=c(rep(0,5),rep(1,5)),corrv=0.9,name="sce4") 

apply(sce4_2$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce4_2$heter_test_sta,2,mean ),3)

#####
sce4_3=sim_varguid(n=200,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=c(rep(0,5),rep(1,5)),corrv=0,name="sce4") 



apply(sce4_3$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce4_3$heter_test_sta,2,mean ),3)
#######
sce4_4=sim_varguid(n=200,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=c(rep(0,5),rep(1,5)),corrv=0.9,name="sce4") 

apply(sce4_4$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce4_4$heter_test_sta,2,mean ),3)

#######################################################
##########
##########scenario5 p=15
#########################################################

sce5_1=sim_varguid(n=20,p=15,beta_real=c(rep(1,5),rep(0,10)),
                   gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,5)),corrv=0,name="sce5") 

apply(sce5_1$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce5_1$heter_test_sta,2,mean ),3)
#####
sce5_2=sim_varguid(n=20,p=15,beta_real=c(rep(1,5),rep(0,10)),
                   gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,5)),corrv=0.9,name="sce5") 

apply(sce5_2$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce5_2$heter_test_sta,2,mean ),3)
#####
sce5_3=sim_varguid(n=200,p=15,beta_real=c(rep(1,5),rep(0,10)),
                   gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,5)),corrv=0,name="sce5") 

apply(sce5_3$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce5_3$heter_test_sta,2,mean ),3)
#####

sce5_4=sim_varguid(n=200,p=15,beta_real=c(rep(1,5),rep(0,10)),
                   gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,5)),corrv=0.9,name="sce5") 

apply(sce5_4$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce5_4$heter_test_sta,2,mean ),3)


########################################################
########## sce1
####################################################

sce1_1=sim_varguid(n=20,p=1,beta_real=1,
                   gamma_real=1,corrv=0,name="sce1") 

apply(sce1_1$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce1_1$heter_test_sta,2,mean ),3)


sce1_3=sim_varguid(n=200,p=1,beta_real=1,
                   gamma_real=1,corrv=0,name="sce1") 

apply(sce1_3$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce1_3$heter_test_sta,2,mean ),3)

########################################################
################# sce3
####################################################

#####
sce3_1=sim_varguid(n=20,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=NULL,corrv=0,name="sce3") 

apply(sce3_1$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce3_1$heter_test_sta,2,mean ),3)
#######
sce3_2=sim_varguid(n=20,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=NULL,corrv=0.9,name="sce3") 

apply(sce3_2$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce3_2$heter_test_sta,2,mean ),3)

######
sce3_3=sim_varguid(n=200,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=NULL,corrv=0,name="sce3") 
round(apply(sce3_3$heter_test_sta,2,mean ),3)
apply(sce3_3$heter_test_p,2,function(x) length(which(x<0.05))/nsim )

#######
sce3_4=sim_varguid(n=200,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=NULL,corrv=0.9,name="sce3") 

apply(sce3_4$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce3_4$heter_test_sta,2,mean ),3)


########################################################
################# sce6
####################################################


sce6_1=sim_varguid(n=20,p=20,beta_real=c(rep(1,5),rep(0,15)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,10)),corrv=0,name="sce6") 
round(mean(sce6_1$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce6_1$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce6_1$res[1:1000,6:20] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce6_1$res[1001:2000,6:20] %>% apply(2,MSE0)),3) # var beta=0


sce6_2=sim_varguid(n=20,p=20,beta_real=c(rep(1,5),rep(0,15)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,10)),corrv=0.9,name="sce6") 
round(mean(sce6_2$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce6_2$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce6_2$res[1:1000,6:20] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce6_2$res[1001:2000,6:20] %>% apply(2,MSE0)),3) # var beta=0



sce6_3=sim_varguid(n=200,p=20,beta_real=c(rep(1,5),rep(0,15)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,10)),corrv=0,name="sce6") 
round(mean(sce6_3$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce6_3$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce6_3$res[1:1000,6:20] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce6_3$res[1001:2000,6:20] %>% apply(2,MSE0)),3) # var beta=0


sce6_4=sim_varguid(n=200,p=20,beta_real=c(rep(1,5),rep(0,15)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,10)),corrv=0.9,name="sce6") 
round(mean(sce6_4$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce6_4$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce6_4$res[1:1000,6:20] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce6_4$res[1001:2000,6:20] %>% apply(2,MSE0)),3) # var beta=0

########################################################
################# sce7
####################################################

sce7_1=sim_varguid(n=20,p=100,beta_real=c(rep(1,5),rep(0,95)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,90)),corrv=0,name="sce7")
round(mean(sce7_1$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce7_1$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce7_1$res[1:1000,6:100] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce7_1$res[1001:2000,6:100] %>% apply(2,MSE0)),3) # var beta=0

sce7_2=sim_varguid(n=20,p=100,beta_real=c(rep(1,5),rep(0,95)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,90)),corrv=0.9,name="sce7") 
round(mean(sce7_2$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce7_2$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce7_2$res[1:1000,6:100] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce7_2$res[1001:2000,6:100] %>% apply(2,MSE0)),3) # var beta=0

sce7_3=sim_varguid(n=200,p=100,beta_real=c(rep(1,5),rep(0,95)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,90)),corrv=0,name="sce7") 

round(mean(sce7_3$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce7_3$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce7_3$res[1:1000,6:100] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce7_3$res[1001:2000,6:100] %>% apply(2,MSE0)),3) # var beta=0


sce7_4=sim_varguid(n=200,p=100,beta_real=c(rep(1,5),rep(0,95)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,90)),corrv=0.9,name="sce7") 
round(mean(sce7_4$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce7_4$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce7_4$res[1:1000,6:100] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce7_4$res[1001:2000,6:100] %>% apply(2,MSE0)),3) # var beta=0

########################################################
################# sce8
####################################################

sce8_1=sim_varguid(n=20,p=200,beta_real=c(rep(1,5),rep(0,195)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,190)),corrv=0,name="sce8",nsim=1000)
round(mean(sce8_1$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce8_1$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce8_1$res[1:1000,6:200] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce8_1$res[1001:2000,6:200] %>% apply(2,MSE0)),3) # var beta=0
sce8_2=sim_varguid(n=20,p=200,beta_real=c(rep(1,5),rep(0,195)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,190)),corrv=0.9,name="sce8",nsim=1000) 

round(mean(sce8_2$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce8_2$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce8_2$res[1:1000,6:200] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce8_2$res[1001:2000,6:200] %>% apply(2,MSE0)),3) # var beta=0


sce8_3=sim_varguid(n=200,p=200,beta_real=c(rep(1,5),rep(0,195)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,190)),corrv=0,name="sce8",nsim=1000) 
round(mean(sce8_3$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce8_3$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce8_3$res[1:1000,6:200] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce8_3$res[1001:2000,6:200] %>% apply(2,MSE0)),3) # var beta=0

sce8_4=sim_varguid(n=200,p=200,beta_real=c(rep(1,5),rep(0,195)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,190)),corrv=0.9,name="sce8",nsim=1000) 
round(mean(sce8_4$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce8_4$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce8_4$res[1:1000,6:200] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce8_4$res[1001:2000,6:200] %>% apply(2,MSE0)),3) # var beta=0

########################################################
################# sce9
####################################################

sce9_1=sim_varguid(n=20,p=400,beta_real=c(rep(1,5),rep(0,395)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,390)),corrv=0,name="sce9",nsim=100)
round(mean(sce9_1$res[1:100,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce9_1$res[101:200,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce9_1$res[1:100,6:400] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce9_1$res[101:200,6:400] %>% apply(2,MSE0)),3) # var beta=0


sce9_2=sim_varguid(n=20,p=400,beta_real=c(rep(1,5),rep(0,395)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,390)),corrv=0.9,name="sce9",nsim=100) 
round(mean(sce9_2$res[1:100,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce9_2$res[101:200,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce9_2$res[1:100,6:400] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce9_2$res[101:200,6:400] %>% apply(2,MSE0)),3) # var beta=0
sce9_3=sim_varguid(n=200,p=400,beta_real=c(rep(1,5),rep(0,395)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,390)),corrv=0,name="sce9",nsim=100) 
round(mean(sce9_3$res[1:100,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce9_3$res[101:200,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce9_3$res[1:100,6:400] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce9_3$res[101:200,6:400] %>% apply(2,MSE0)),3) # var beta=0
sce9_4=sim_varguid(n=200,p=400,beta_real=c(rep(1,5),rep(0,395)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,390)),corrv=0.9,name="sce9",nsim=100) 
round(mean(sce9_3$res[1:100,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce9_3$res[101:200,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce9_3$res[1:100,6:400] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce9_3$res[101:200,6:400] %>% apply(2,MSE0)),3) # var beta=0

########################################################
################# sce5 lasso
####################################################


data=readRDS("../varguid data/20240601 simulated data/sce5_20with15with0.rds")
sce5_1_lasso=varguid(data,lasso_status = TRUE)

round(mean(sce5_1_lasso$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce5_1_lasso$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce5_1_lasso$res[1:1000,6:15] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce5_1_lasso$res[1001:2000,6:15] %>% apply(2,MSE0)),3) # var beta=0



data2=readRDS("../varguid data/20240601 simulated data/sce5_20with15with0.9.rds")
sce5_2_lasso=varguid(data2,lasso_status = TRUE)
round(mean(sce5_2_lasso$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce5_2_lasso$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce5_2_lasso$res[1:1000,6:15] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce5_2_lasso$res[1001:2000,6:15] %>% apply(2,MSE0)),3) # var beta=0


data3=readRDS("../varguid data/20240601 simulated data/sce5_200with15with0.rds")
sce5_3_lasso=varguid(data3,lasso_status = TRUE)

round(mean(sce5_3_lasso$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce5_3_lasso$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce5_3_lasso$res[1:1000,6:15] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce5_3_lasso$res[1001:2000,6:15] %>% apply(2,MSE0)),3) # var beta=0



data4=readRDS("../varguid data/20240601 simulated data/sce5_200with15with0.9.rds")
sce5_4_lasso=varguid(data4,lasso_status = TRUE)

round(mean(sce5_4_lasso$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce5_4_lasso$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce5_4_lasso$res[1:1000,6:15] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce5_4_lasso$res[1001:2000,6:15] %>% apply(2,MSE0)),3) # var beta=0

