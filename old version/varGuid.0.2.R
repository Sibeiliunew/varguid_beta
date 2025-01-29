AICf <- function(r, p, w = NULL, step = NULL){
  n <- length(r)
  Sig <- sqrt(sum(r^2)/(n-p))
  if (is.null(w)) {
  loglkhd <- -n*log(2*pi)/2-n*log(Sig)-(n-p)/2
  } else {
    loglkhd <- -n*log(2*pi)/2-n*log(Sig)-(n-p)*sum(r^2*exp(-step*w))/2/(Sig^2)/n
  }
  -2*loglkhd + 2*p
}


beta_est=function(X,Y,w, step = 1){
  w_new <- w/max(w[1])
  o <- lm(outcome~.,data = data.frame(X=X,outcome=Y), weights = exp(-step*w_new))
  beta <- coef(o)
  if (length(unique(w_new))>1){
    p <- 2*(ncol(X))
    AIC <- AICf(r = o$residuals, p, w = w_new, step = step)
  } else {
    p <- ncol(X)
    AIC <- AICf(r = o$residuals, p)
    }

  return(list(beta = beta, AIC = AIC))
}
w_est=function(X,Y,beta){
  o <- lm(outcome~.,data = data.frame(X = X^2,outcome = (Y-cbind(1,X)%*% beta )^2))
  r <- o$fitted.values

  # gamma <- coef(o)
  return(w = r)
}
##### check if the above result is correct
lmv <- function(X, Y, M =  100, step = 1, tol = exp(-10)){
n <- length(Y)
diff1 <- step

o1 <- beta_est(X,Y,w = rep(1,nrow(X)))
beta <- beta1 <- o1$beta
AIC <- o1$AIC
#allstuff <- c(beta, AIC = AIC,MSE = sum(abs(beta[-1]-rep(-1,5))))
for (i in 1:M) {


  old_beta <- beta
  old_AIC <- AIC
  w <- w_est(X,Y,beta)
  o <- beta_est(X,Y,w, step = step)
  if ((diff1 > tol) & (o$AIC < old_AIC))  {  #& (o$AIC < old_AIC))
    beta <- o$beta
    AIC <- o$AIC
 #   allstuff <- cbind(allstuff, c(beta,AIC = AIC, MSE = sum(abs(beta[-1]-rep(-1,5)))))
  } else {
    step <- 0.1*step
    next
  }
  diff1=sum((beta-old_beta)^2)

}

#list(beta = beta, allstuff = allstuff)
beta
}


