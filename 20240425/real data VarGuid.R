library(tidyverse)
library(janitor)
library(MASS)
library(xtable)
library(scales)
library(caTools)
library(readxl)
library(openxlsx)
library(mlbench)
library("caret")
library("e1071")
library(knitr)
options(digits=4)

source("./20240425/VarGuid20240502.R")
source("./20240425/leash2.0.2.R")


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

y.obj<- ymodv(o,gamma = c(seq(0,8.56, length.out=5)), phi = 0.45,rf = FALSE)

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
       "./20240424/mcs_ds_edited_iter_shuffled.xlsx",
       "./20240424/auto.xlsx", 
       "./20240424/slump_final.xlsx",
       "./20240424/chem.xlsx",
      #"./20240424/sevo.xlsx",
      "./20240424/for.xlsx",
      "./20240424/fb.xlsx")

rmse <- c()
rmse_res=NULL

for (d in 1:length(path)){
  real= read_excel(path[d]) %>% janitor::clean_names() 
  real = na.omit(real)
  real <- cbind(makeX(real[,1:(ncol(real)-1)]), real[,ncol(real)])
  folds=createFolds(1:nrow(real), k = 10) ## 10 cv
  for (i in 1:10){
  print(i) 
  #trn <- sample.split(1:nrow(real), SplitRatio = 0.75)
  
  train  <- real[-folds[[i]],]
  test   <- real[folds[[i]],]
  
  data=list(x.train = train[,1:(ncol(real)-1)],
            y.train = train[,ncol(real)],
            x.test = test[,1:(ncol(real)-1)],
            y.test = test[,ncol(real)])
  
  o <- lmv(X =as.matrix(data$x.train) , Y = unlist(data$y.train), lasso = FALSE) # , lasso = TRUE
   y.obj <-ymodv(o,gamma = c(seq(0,9, length.out=5)), phi = 0.46)#, rf = FALSE)
  
  
  pred <- fnpred(mod=y.obj,lmvo = o,newdata = data$x.test)
  
  rmse <- rbind(rmse,sqrt(colMeans((matrix(rep(data$y.test,ncol(pred)),length(data$y.test))-pred)^2)) )
  }
  rmse_res[[d]]=colMeans(as.data.frame(rmse))
}
table1=do.call("rbind",rmse_res)
table1
save(table1, file = "rmse_resMin18Leash2.0.RData")
