library(tidyverse)
library(janitor)
library("tidyverse") 
library(MASS)
library(xtable)
library(scales)
library(caTools)
library(readxl)
library(openxlsx)
library(mlbench)

source("./VarGuid20240626.R")
source("./20240412/leash2.0.R")


flash <- readRDS("flash.rds")
dat=data.frame(MSV_exp= flash$MSV_exp,MSV_insp=flash$MSV_insp,VH_exp= flash$VH_exp,VH_insp=flash$VH_insp,
                       VDP_exp= flash$VDP_exp, VDP_insp=flash$VDP_insp, TV_exp= flash$TV_exp, TV_insp=flash$TV_insp,FEV1=flash$best_pre_fev1,
                       age=flash$age,bmi=flash$bmi_new,cat_score=flash$cat_score
) %>% drop_na()

predictor=dat[,1:11]


cat_res=lm(cat_score~.,data = dat)

plot(cat_res$fitted.values,cat_res$residuals^2) 
title("The sigma^2 estimation")

plot(cat_res$fitted.values,cat_res$residuals) 
title("Residual Variance")


### bootstrap data and split data into train and test

boot_num=10
resamples =lapply(1:boot_num, function(i) dat[sample(1:nrow(dat), size=nrow(dat),replace = T),]) 
resamples=do.call(rbind.data.frame, resamples)
resamples$id=seq(1,nrow(resamples),length.out=nrow(resamples))

sample <- sample.split(resamples$id, SplitRatio = 0.75)
train  <- subset(resamples, sample == TRUE)
test   <- subset(resamples, sample == FALSE)
train.x=train[,1:11]
train.y=train$cat_score
test.x=test[,1:11]
test.y=test$cat_score
####

o=lmv(X=apply(train.x,2,as.numeric),Y=train.y)

y.obj<- ymodv(o,gamma = c(seq(0,9, length.out=5)), phi = 0.46)

yhat.varGuid <- fnpred(mod=y.obj,
                       lmvo = o,
                       newdata = o$obj.varGuid$model[1:5,2:(ncol(o$obj.varGuid$model)-1)]) 


### test error
pred <- fnpred(mod=y.obj,lmvo = o,newdata = test.x)
rmse <- c()
rmse <- rbind(rmse,sqrt(colMeans((matrix(rep(test.y,ncol(pred)),length(test.y))-pred)^2)) )
### test error
rmse

####


##### UCI data
### 1. concrete
path=c("./20240424/concrete+compressive+strength/Concrete_Data.xls",
       "./20240424/liver.xlsx",
       "./20240424/Airfoil.xlsx",
       "./20240424/Real estate valuation data set.xlsx",
       "./20240424/mcs_ds_edited_iter_shuffled.xlsx")
# flash <- readRDS("flash.data.1003.rds")
# real=data.frame(MSV_exp= flash$MSV_exp,MSV_insp=flash$MSV_insp,VH_exp= flash$VH_exp,VH_insp=flash$VH_insp,
#                VDP_exp= flash$VDP_exp, VDP_insp=flash$VDP_insp, TV_exp= flash$TV_exp, TV_insp=flash$TV_insp,FEV1=flash$best_pre_fev1,
#                age=flash$age,bmi=flash$bmi_new,cat_score=flash$cat_score
# ) %>% drop_na()

# rmse <- c()
# rmse_res=NULL

coef.table=NULL

for (d in 1:5){
  real=read_excel(path[d]) %>% janitor::clean_names()
  for (i in 1:5){
   print(i) 
  #   
  #   
  #   
  # trn <- sample.split(1:nrow(real), SplitRatio = 0.75)
  # 
  # train  <- subset(real, trn == TRUE)
  # test   <- subset(real, trn== FALSE)
  # 
  # data=list(x.train = train[,1:(ncol(real)-1)],
  #           y.train = train[,ncol(real)],
  #           x.test = test[,1:(ncol(real)-1)],
  #           y.test = test[,ncol(real)])
  # 
  # o <- lmv(X = data$x.train, Y = unlist(data$y.train))
  #  y.obj <-ymodv(o,gamma = c(seq(0,9, length.out=5)), phi = 0.46)
  # 
  # 
  # pred <- fnpred(mod=y.obj,lmvo = o,newdata = data$x.test)
  # 
  # rmse <- rbind(rmse,sqrt(colMeans((matrix(rep(data$y.test,ncol(pred)),length(data$y.test))-pred)^2)) )
    
    x=
    
    
  ##### for var-guild lasso
    o1 <- lmv(X =as.matrix() , Y = unlist(), lasso = TRUE)
    m=as.data.frame(as.matrix(o1$beta)) %>% filter(s0>0)
    n1=nrow(m)
    select1=rownames(m)
  ### for lasso
    o2 <- cv.glmnet(x =as.matrix() , y = ,alpha = 1,lambda =exp(seq(-1,1,length=100) ))
    #plot(o_p20133_2)
    m2=as.data.frame(as.matrix(coef(o2, s = "lambda.min"))) %>% filter(s1>0)
    n2=nrow(m2)
    select2=rownames(m2)
    n_ol=length(intersect(n1,n2))
    
  }
  coef.table[[d]]=c(n1,n2,n_ol)
}
do.call("rbind",coef.table)


