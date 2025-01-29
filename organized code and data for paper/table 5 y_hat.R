
source("./leash2.0.6.R")
source("./VarGuid20240626.R")
source('./table simulation functions.R')
library(glmnet)
library(tidyverse)
library(MASS)
library(xtable)
library(scales)
library(faux)
library(olsrr)
###### re-simulate new data with same dim to get y hat

###############
dat <- readRDS("../varguid data/20240601 simulated data/sce1_20with1with0.rds")
test=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
             beta_real=1,
             gamma_real=NULL,corrv=0)
yhat(dat = dat,test=test,lasso_status=FALSE)


dat <- readRDS("../varguid data/20240601 simulated data/sce1_200with1with0.rds")
test=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
             beta_real=1,
             gamma_real=NULL,corrv=0)
yhat(dat = dat,test=test,lasso_status=FALSE)


###################################################
###################################################
dat <- readRDS("../varguid data/20240601 simulated data/sce2_20with10with0.rds")
test=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
             beta_real=c(rep(1,5),rep(0,5)),
             gamma_real=c(rep(1,5),rep(0,5)),corrv=0)
yhat(dat = dat,test=test,lasso_status=FALSE)


dat <- readRDS("../varguid data/20240601 simulated data/sce2_20with10with0.9.rds")
test=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
             beta_real=c(rep(1,5),rep(0,5)),
             gamma_real=c(rep(1,5),rep(0,5)),corrv=0.9)
yhat(dat = dat,test=test,lasso_status=FALSE)

##########################################################################
#########################################################################

dat <- readRDS("../varguid data/20240601 simulated data/sce3_20with10with0.rds")
test=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
             beta_real=c(rep(1,5),rep(0,5)),
             gamma_real=NULL,corrv=0)
yhat(dat = dat,test=test,lasso_status=FALSE)


dat <- readRDS("../varguid data/20240601 simulated data/sce3_20with10with0.9.rds")
test=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
             beta_real=c(rep(1,5),rep(0,5)),
             gamma_real=NULL,corrv=0.9)
yhat(dat = dat,test=test,lasso_status=FALSE)

##########################################################################
#########################################################################

dat <- readRDS("../varguid data/20240601 simulated data/sce4_20with10with0.rds")
test=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
             beta_real=c(rep(1,5),rep(0,5)),
             gamma_real=c(rep(0,5),rep(1,5)),corrv=0)
yhat(dat = dat,test=test,lasso_status=FALSE)


dat <- readRDS("../varguid data/20240601 simulated data/sce4_20with10with0.9.rds")
test=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
             beta_real=c(rep(1,5),rep(0,5)),
             gamma_real=c(rep(0,5),rep(1,5)),corrv=0.9)
yhat(dat = dat,test=test,lasso_status=FALSE)

##########################################################################
#########################################################################


dat <- readRDS("../varguid data/20240601 simulated data/sce5_20with15with0.rds")
#test_sce5_20with15with0=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
#                                 beta_real=c(rep(1,5),rep(0,10)),
#                                 gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,5)),corrv=0)

test_sce5_20with15with0=readRDS("../varguid data/test_sce5_20with15with0.rds")

yhat(dat = dat,test=test_sce5_20with15with0,lasso_status=FALSE)


dat <- readRDS("../varguid data/20240601 simulated data/sce5_20with15with0.9.rds")
#test_sce5_20with15with0.9=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
#                                   beta_real=c(rep(1,5),rep(0,10)),
#                                   gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,5)),corrv=0.9)
#saveRDS(test_sce5_20with15with0.9,"test_sce5_20with15with0.9.rds")

test_sce5_20with15with0.9=readRDS("../varguid data/test_sce5_20with15with0.9.rds")
yhat(dat = dat,test=test_sce5_20with15with0.9,lasso_status=FALSE)




dat <- readRDS("../varguid data/20240601 simulated data/sce5_200with15with0.rds")
# test_sce5_200with15with0=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
#              beta_real=c(rep(1,5),rep(0,10)),
#              gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,5)),corrv=0)
# #saveRDS(test_sce5_200with15with0,"test_sce5_200with15with0.rds")
test_sce5_200with15with0=readRDS("../varguid data/test_sce5_200with15with0.rds")
yhat(dat = dat,test=test_sce5_200with15with0,lasso_status=FALSE)


dat <- readRDS("../varguid data/20240601 simulated data/sce5_200with15with0.9.rds")
# test_sce5_200with15with0.9=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
#              beta_real=c(rep(1,5),rep(0,10)),
#              gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,5)),corrv=0.9)

#saveRDS(test_sce5_200with15with0.9,"test_sce5_200with15with0.9.rds")

test_sce5_200with15with0.9=readRDS("../varguid data/test_sce5_200with15with0.9.rds")

yhat(dat = dat,test=test_sce5_200with15with0.9,lasso_status=FALSE)


#################################
yhat(dat = readRDS("./20240601 simulated data/sce5_20with15with0.rds"),test=test_sce5_20with15with0,lasso_status=TRUE)
yhat(dat = readRDS("./20240601 simulated data/sce5_20with15with0.9.rds"),test=test_sce5_20with15with0.9,lasso_status=TRUE)
yhat(dat = readRDS("./20240601 simulated data/sce5_200with15with0.rds"),test=test_sce5_200with15with0,lasso_status=TRUE)
yhat(dat = readRDS("./20240601 simulated data/sce5_200with15with0.9.rds"),test=test_sce5_200with15with0.9,lasso_status=TRUE)

#############################################
#############################################
yhat(dat = readRDS("./sce6_20with20with0.rds"),test=dat_sim(n=20,p=20,
                                                            beta_real=c(rep(1,5),rep(0,15)),
                                                            gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,10)),corrv=0),lasso_status=TRUE)
yhat(dat = readRDS("./sce6_20with20with0.9.rds"),test=dat_sim(n=20,p=20,
                                                              beta_real=c(rep(1,5),rep(0,15)),
                                                              gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,10)),corrv=0.9),lasso_status=TRUE)
yhat(dat = readRDS("./sce6_200with20with0.rds"),test=dat_sim(n=20,p=20,
                                                             beta_real=c(rep(1,5),rep(0,15)),
                                                             gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,10)),corrv=0),lasso_status=TRUE)
yhat(dat = readRDS("./sce6_200with20with0.9.rds"),test=dat_sim(n=20,p=20,
                                                               beta_real=c(rep(1,5),rep(0,15)),
                                                               gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,10)),corrv=0.9),lasso_status=TRUE)
#############################################
#############################################
yhat(dat = readRDS("../varguid data/sce7_20with100with0.rds"),test=dat_sim(n=20,p=100,
                                                                           beta_real=c(rep(1,5),rep(0,95)),
                                                                           gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,90)),corrv=0),lasso_status=TRUE)
yhat(dat = readRDS("../varguid data/sce7_20with100with0.9.rds"),test=dat_sim(n=20,p=100,
                                                                             beta_real=c(rep(1,5),rep(0,95)),
                                                                             gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,90)),corrv=0.9),lasso_status=TRUE)
yhat(dat = readRDS("../varguid data/sce7_200with100with0.rds"),test=dat_sim(n=20,p=100,
                                                                            beta_real=c(rep(1,5),rep(0,95)),
                                                                            gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,90)),corrv=0),lasso_status=TRUE)
yhat(dat = readRDS("../varguid data/sce7_200with100with0.9.rds"),test=dat_sim(n=20,p=100,
                                                                              beta_real=c(rep(1,5),rep(0,95)),
                                                                              gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,90)),corrv=0.9),lasso_status=TRUE)

#############################################
#############################################
yhat(dat = readRDS("../varguid data/sce8_20with200with0.rds"),test=dat_sim(n=20,p=200,
                                                                           beta_real=c(rep(1,5),rep(0,195)),
                                                                           gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,190)),corrv=0),lasso_status=TRUE)
yhat(dat = readRDS("../varguid data/sce8_20with200with0.9.rds"),test=dat_sim(n=20,p=200,
                                                                             beta_real=c(rep(1,5),rep(0,195)),
                                                                             gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,190)),corrv=0.9),lasso_status=TRUE)
yhat(dat = readRDS("../varguid data/sce8_200with200with0.rds"),test=dat_sim(n=20,p=200,
                                                                            beta_real=c(rep(1,5),rep(0,195)),
                                                                            gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,190)),corrv=0),lasso_status=TRUE)
yhat(dat = readRDS("../varguid data/sce8_200with200with0.9.rds"),test=dat_sim(n=20,p=200,
                                                                              beta_real=c(rep(1,5),rep(0,195)),
                                                                              gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,190)),corrv=0.9),lasso_status=TRUE)
#############################################
#############################################

yhat(dat = readRDS("./sce9_20with400with0.rds"),test=dat_sim(n=20,p=400,
                                                             beta_real=c(rep(1,5),rep(0,395)),
                                                             gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,390)),corrv=0),lasso_status=TRUE)
yhat(dat = readRDS("./sce9_20with400with0.9.rds"),test=dat_sim(n=20,p=400,
                                                               beta_real=c(rep(1,5),rep(0,395)),
                                                               gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,390)),corrv=0.9),lasso_status=TRUE)
yhat(dat = readRDS("./sce9_200with400with0.rds"),test=dat_sim(n=200,p=400,
                                                              beta_real=c(rep(1,5),rep(0,395)),
                                                              gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,390)),corrv=0),lasso_status=TRUE)
yhat(dat = readRDS("./sce9_200with400with0.9.rds"),test=dat_sim(n=200,p=400,
                                                                beta_real=c(rep(1,5),rep(0,395)),
                                                                gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,390)),corrv=0.9),lasso_status=TRUE)

