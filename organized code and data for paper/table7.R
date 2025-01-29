library(tidyverse)
library(janitor)
library(MASS)
library(xtable)
library(scales)
library(caTools)
library(readxl)
library(openxlsx)
library(mlbench)

source("./20240412/VarGuid20240626.R")
source("./leash2.0.7.R")


##### UCI data
### 1. concrete
path=c("./20240424/concrete+compressive+strength/Concrete_Data.xls",
       "./20240424/liver.xlsx",
       "./20240424/Airfoil.xlsx",
       "./20240424/Real estate valuation data set.xlsx",
       "./20240424/mcs_ds_edited_iter_shuffled.xlsx")

rmse <- c()
rmse_res=NULL

for (d in 1:length(path)){
  real=read_excel(path[d]) %>% janitor::clean_names()
  for (i in 1:5){
  print(i) 
  trn <- sample.split(1:nrow(real), SplitRatio = 0.75)
  
  train  <- subset(real, trn == TRUE)
  test   <- subset(real, trn== FALSE)
  
  data=list(x.train = train[,1:(ncol(real)-1)],
            y.train = train[,ncol(real)],
            x.test = test[,1:(ncol(real)-1)],
            y.test = test[,ncol(real)])
  
  o <- lmv(X = data$x.train, Y = unlist(data$y.train))
   y.obj <-ymodv(o,gamma = c(seq(0,9, length.out=5)), phi = 0.46)
  
  
  pred <- fnpred(mod=y.obj,lmvo = o,newdata = data$x.test)
  
  rmse <- rbind(rmse,sqrt(colMeans((matrix(rep(data$y.test,ncol(pred)),length(data$y.test))-pred)^2)) )
  }
  rmse_res[[d]]=colMeans(as.data.frame(rmse))
}
do.call("rbind",rmse_res)


