library(VarGuid)

source("leash2.0.7.R")
source("varGuid20240626.R")


cobra2 = function(n = 1000, d = 10, sd = .1, corrv = 0) {
  set.seed(1)
  d <- max(10, d)
  X <- matrix(runif(n * d, -1, 1), ncol = d)
  paramlist <- lapply(1:d, function(j) {list(min=-1,max=1)})
  myCop <- normalCopula(param=rep(corrv,dim(combn(d,2))[2]), dim = d, dispstr = "un")
  myMvd <-  mvdc(copula=myCop, margins=rep("unif",d),paramMargins=paramlist)
  X[, 1:d] <- rMvdc(n, myMvd)
  dta <- data.frame(list(x = X, y = X[,1]*X[,2] + X[,3]^2 - X[,4]*X[,7] + X[,8]*X[,10] - X[,6]^2
                         + rnorm(n, sd = sd)))
  colnames(dta)[1:d] <- paste("x", 1:d, sep = "")
  f <- "x1 * x2 + x3 ^ 2 - x4 * x7 + x8 * x10 - x6 ^ 2"
  fs <- "I(x1 * x2) + I(x3 ^ 2) + I(-x4 * x7) + I(x8 * x10) - I(x6 ^ 2)"
  list(f = f, fs = fs, dta = dta)
}

rmse <- function(dat,  yid = ncol(dat), lasso = FALSE){
  train <- dat[1:round(nrow(dat)*0.8),]
  test <- dat[-c(1:round(nrow(dat)*0.8)),]

  
  o <- lmv(X = train[,-yid] , Y = train[,yid], lasso = lasso) # , lasso = TRUE
  y.obj <- ymodv(obj = o,gamma = c(seq(0,9, length.out=5)), phi = 0.46)#, rf = FALSE)
  
  
  pred <- fnpred(mod=y.obj,lmvo = o,newdata = test[,-yid])
  
  sqrt(mean((test[,yid]-pred[,4])^2)) 
}
## Table 7
cobra2d <- cobra2(n = 500, d = 15, sd = .1, corrv = 0)
cobra2dstar <- cobra2(n = 500, d = 15, sd = .1, corrv = 0.9)

rmse(dat = cobra2d$dta)
rmse(dat = cobra2dstar$dta)
## Table 8
cobra2d <- cobra2(n = 100, d = 200, sd = .1, corrv = 0)
cobra2dstar <- cobra2(n = 100, d = 200, sd = .1, corrv = 0.9)

rmse(dat = cobra2d$dta, lasso = TRUE)
rmse(dat = cobra2dstar$dta, lasso = TRUE)


