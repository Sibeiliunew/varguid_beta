library(lmtest)
library(sandwich)

AICf <- function(r, p){
  n <- length(r)
  Sig <- sum(r^2)/(n-p)
  loglkhd <- -n*log(2*pi)/2-n*log(sqrt(Sig))-(n-p)/2
  -2*loglkhd + 2*p
}


beta_est=function(X, Y, w, step = 1){
  o <- lm(Y~.,data = data.frame(X,Y=Y), weights = exp(-step*w))
  beta <- coef(o)
  
  if (length(unique(w))>1){
    p <- 2*(ncol(X))
  } else { p <- ncol(X) }
  AIC <- AICf(r = o$residuals, p)
  return(list(beta = beta, AIC = AIC, obj = o))
}
w_est=function(X,beta_obj){
  o <- lm(Y~.,data = data.frame(X = X^2,Y = (beta_obj$residuals)^2))
  
  r <- o$fitted.values
  m <- max(r)[1]
  # gamma <- coef(o)
  return(list(w = r/m, res = o))
}
##### check if the above result is correct
lmv <- function(X, Y, M =  100, step = 1, tol = exp(-10)){
n <- length(Y)
diff1 <- step

o1 <- o <- beta_est(X, Y, w = rep(1,nrow(X)))
beta <- beta1 <- o1$beta
obj.OLS <- o1$obj
AIC <- o1$AIC

for (i in 1:M) {


  old_beta <- beta
  old_AIC <- AIC
  res <- w_est(X,beta_obj = o$obj)
  w <- res$w
  o <- beta_est(X, Y,w, step = step)
  if (diff1 > tol) {
    beta <- o$beta
    obj.varGuid <- o$obj
    AIC <- o$AIC

  } else {
    step <- 0.1*step
    next
  }
  diff1=sum((beta-old_beta)^2)

}

obj.coef <- list()
obj.coef$MLE <- obj.varGuid
obj.coef$HC3 <- coeftest(obj.varGuid, vcov = vcovHC(obj.varGuid, "HC3"))
obj.coef$HC <- coeftest(obj.varGuid, vcov = vcovHC(obj.varGuid, "HC"))
obj.coef$HC0 <- coeftest(obj.varGuid, vcov = vcovHC(obj.varGuid, "HC0"))
obj.coef$HC1 <- coeftest(obj.varGuid, vcov = vcovHC(obj.varGuid, "HC1"))
obj.coef$HC2 <- coeftest(obj.varGuid, vcov = vcovHC(obj.varGuid, "HC2"))
obj.coef$HC4 <- coeftest(obj.varGuid, vcov = vcovHC(obj.varGuid, "HC4"))
obj.coef$HC4m <- coeftest(obj.varGuid, vcov = vcovHC(obj.varGuid, "HC4m"))
obj.coef$HC5 <- coeftest(obj.varGuid, vcov = vcovHC(obj.varGuid, "HC5"))
  

list(beta=beta, obj.OLS = obj.OLS, obj.varGuid = obj.varGuid, res = res$res, obj.varGuid.coef = obj.coef)
}


