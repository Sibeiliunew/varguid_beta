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

## 8
library(datamicroarray)
data('west', package = 'datamicroarray')
dat <- data.frame(west$x[,-132],y=west$x[,132])
#rmse(dat = dat, lasso = TRUE)
table_real8=NULL
rm(res)

  while ( nrow(table_real8) < 100){
  skip_to_next <- FALSE
  tryCatch(res<- rmse(dat = dat, lasso = TRUE)$varguid,
           error = function(e) { message('Caught an error!')
             print(e)
             skip_to_next = TRUE})
  
  if(skip_to_next) { next } 
  table_real8=rbind(table_real8,res)
  }

colMeans(table_real8,na.rm = T)
######10
data('subramanian', package = 'datamicroarray')
dat <- data.frame(subramanian$x[,-which(colnames(subramanian$x)=="BAX")],y=subramanian$x[,"BAX"])
table_real10=NULL
rm(res)
while ( nrow(table_real10) < 100){
  skip_to_next <- FALSE
  tryCatch(res<- rmse(dat = dat, lasso = TRUE)$varguid,
           error = function(e) { message('Caught an error!')
             print(e)
             skip_to_next = TRUE})
  
  if(skip_to_next) { next } 
  table_real10=rbind(table_real10,res)
}
colMeans(table_real10,na.rm = T)

######5
data('shipp', package = 'datamicroarray')
dat <- data.frame(shipp$x[,-which(colnames(shipp$x)=="V2006")],y=shipp$x[,"V2006"])
table_real5=NULL
rm(res)

  res<- rmse(dat = dat, lasso = TRUE)$varguid 
  table_real5=rbind(table_real5,res)

while ( nrow(table_real5) < 100){
  skip_to_next <- FALSE
  tryCatch(res<- rmse(dat = dat, lasso = TRUE)$varguid,
           error = function(e) { message('Caught an error!')
             print(e)
             skip_to_next = TRUE})
  
  if(skip_to_next) { next } 
  table_real5=rbind(table_real5,res)
}

colMeans(table_real5,na.rm = T)

######4
data('pomeroy', package = 'datamicroarray')
dat <- data.frame(pomeroy$x[,-which(colnames(pomeroy$x)=="D28473-s-at")],y=pomeroy$x[,"D28473-s-at"])
table_real4=NULL
rm(res)

  res<- rmse(dat = dat, lasso = TRUE)$varguid # 10 repeated train-test
  table_real4=rbind(table_real4,res)

while ( nrow(table_real4) < 100){
  skip_to_next <- FALSE
  tryCatch(res<- rmse(dat = dat, lasso = TRUE)$varguid,
           error = function(e) { message('Caught an error!')
             print(e)
             skip_to_next = TRUE})
  
  if(skip_to_next) { next } 
  table_real4=rbind(table_real4,res)
}

colMeans(table_real4,na.rm = T)
