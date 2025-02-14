library(MASS)
library(copula)

#library(devtools)
#install_github("ramhiser/datamicroarray")

source("leash2.0.9.R")
source("varGuid20250212.R")


rmse <- function(dat,  yid = ncol(dat), lasso = FALSE){
  return <- list()
 dat <- as.data.frame(scale(dat))
  train <- dat[1:round(nrow(dat)*0.8),]
  test <- dat[-c(1:round(nrow(dat)*0.8)),]
  X = train[,-yid]
  Y = train[,yid]
  
  o <- lmv(X = X , Y = Y, lasso = lasso) # , lasso = TRUE
  y.obj <- ymodv(obj = o,gamma = c(seq(0,9, length.out=5)), phi = 0.46)#, rf = FALSE)
  
  pred <- fnpred(mod=y.obj,lmvo = o,newdata = test[,-yid])
  
  return$varguid <- sqrt(colMeans((  matrix(replicate(ncol(pred),test[,yid]),ncol=ncol(pred))-pred)^2, na.rm = TRUE)) 

  
  cv_model <- cv.glmnet(x = as.matrix(train[,-yid]) , y = train[,yid], alpha = 1, 
                        nfolds = 10)

  #find optimal lambda value that minimizes test MSE
  best_lambda <- cv_model$lambda.min
  o <- glmnet(x = as.matrix(train[,-yid]) , y = train[,yid], alpha = 1, lambda = best_lambda)
  
  pred <- predict(o,newx = as.matrix(test[,-yid]))
  
  return$lasso <- sqrt(mean((test[,yid]-pred)^2)) 
  return
}

## Table 8 alone
library(datamicroarray)

data('alon', package = 'datamicroarray')
dat <- data.frame(alon$x[,-which(colnames(alon$x)=="X765")],y=alon$x[,"X765"])


rmse(dat = dat,  lasso = TRUE)

result1 <- lapply(1:10, function(i) {rmse(dat = dat, lasso = TRUE)$varguid})
colMeans(do.call(rbind,result1))


data('gordon', package = 'datamicroarray')
dat <- data.frame(x=gordon$x[,-which(colnames(gordon$x)=="34320_at")],y=gordon$x[,"34320_at"])

rmse(dat = dat,  yid = ncol(dat), lasso = TRUE)

result1 <- lapply(1:10, function(i) {rmse(dat = dat, lasso = TRUE)$varguid})
colMeans(do.call(rbind,result1))


data('christensen', package = 'datamicroarray')
dat <- data.frame(christensen$x[,-which(colnames(christensen$x)=="OSM_P188_F")],y=christensen$x[,"OSM_P188_F"])
result <- lapply(1:10, function(i) {rmse(dat = dat, lasso = TRUE)$varguid})
colMeans(do.call(rbind,result))

data('pomeroy', package = 'datamicroarray')
dat <- data.frame(pomeroy$x[,-which(colnames(pomeroy$x)=="D28473-s-at")],y=pomeroy$x[,"D28473-s-at"])
result2 <- lapply(1:2, function(i) {rmse(dat = dat, lasso = TRUE)$varguid})
colMeans(do.call(rbind,result2))

data('tian', package = 'datamicroarray')
dat <- data.frame(tian$x[,-which(colnames(tian$x)=="898_s_at")],y=tian$x[,"898_s_at"])
result <- lapply(1:2, function(i) {rmse(dat = dat, lasso = TRUE)$varguid})
colMeans(do.call(rbind,result))


