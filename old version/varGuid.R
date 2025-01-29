AICf <- function(r, p){
  n <- length(r)
  Sig <- sum(r^2)/(n-p)
  loglkhd <- -n*log(2*pi)/2-n*log(sqrt(Sig))-(n-p)/2
  -2*loglkhd + 2*p
}


beta_est=function(X, Y, w, step = 1){
  o <- lm(outcome~.,data = data.frame(X=X,outcome=Y), weights = exp(-step*w))
  beta <- coef(o)
  if (length(unique(w))>1){
    p <- 2*(ncol(X))
  } else { p <- ncol(X) }
  AIC <- AICf(r = o$residuals, p)
  return(list(beta = beta, AIC = AIC))
}
w_est=function(X,Y,beta){
  o <- lm(outcome~.,data = data.frame(X = X^2,outcome = (Y-cbind(1,X)%*% beta )^2))
  r <- o$fitted.values
  m <- max(r)[1]
  # gamma <- coef(o)
  return(w = r/m)
}
##### check if the above result is correct
lmv <- function(X, Y, M =  100, step = 1, tol = exp(-10)){
n <- length(Y)
diff1 <- step

o1 <- beta_est(X, Y, w = rep(1,nrow(X)))
beta <- beta1 <- o1$beta
AIC <- o1$AIC

for (i in 1:M) {


  old_beta <- beta
  old_AIC <- AIC
  w <- w_est(X,Y,beta)
  o <- beta_est(X, Y,w, step = step)
  if (diff1 > tol) {
    beta <- o$beta
    AIC <- o$AIC

  } else {
    step <- 0.1*step
    next
  }
  diff1=sum((beta-old_beta)^2)

}
se=sqrt(sum((Y-cbind(1,X)%*% beta )^2)/(n-ncol(X)-1)*diag(solve(crossprod( diag(w) %*%cbind(1,X)))))
return(list(beta=beta,se=se))
}


