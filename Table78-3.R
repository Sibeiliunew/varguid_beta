
library(copula)
source("leash2.0.8.R")
source("varGuid20250209.R")

sup = function(n = 1000, d = 6, sd = .5, corrv = 0) {
  d <- max(6, d)
  X <- matrix(runif(n * d, .05, 1), ncol = d)
  paramlist <- lapply(1:d, function(j) {list(min=.05,max=1)})
  myCop <- normalCopula(param=rep(corrv,dim(combn(d,2))[2]), dim = d, dispstr = "un")
  myMvd <- mvdc(copula=myCop, margins=rep("unif",d),paramMargins=paramlist)
  X[, 1:d] <- rMvdc(n, myMvd)
  y <- 10 * X[,1] * X[,2] + .25 / (X[,3] * X[,4]) + 10 * X[,5] * X[,6] + rnorm(n, sd = sd)
  dta <- data.frame(list(x = X, y = y))
  colnames(dta)[1:d] <- paste("x", 1:d, sep = "")
  f <- "10*x1*x2 + 0.25/(x3*x4) + 10*x5*x6"
  fs <- "I(10 * x1 * x2) + I(0.25 / (x3 * x4)) + I(10 * x5 *x6)"
  list(f = f, fs = fs, dta = dta)
}

rmse <- function(dat,  yid = ncol(dat), lasso = FALSE){
  return <- list()
  train <- dat[1:round(nrow(dat)*0.8),]
  test <- dat[-c(1:round(nrow(dat)*0.8)),]

  
  o <- lmv(X = train[,-yid] , Y = train[,yid], lasso = lasso) # , lasso = TRUE
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

## Table 8 sup
cobra2d <- sup(n = 100, d = 200, sd = .1, corrv = 0)
cobra2dstar <- sup(n = 100, d = 200, sd = .1, corrv = 0.9)

rmse(dat = cobra2d$dta, lasso = TRUE)
rmse(dat = cobra2dstar$dta, lasso = TRUE)


