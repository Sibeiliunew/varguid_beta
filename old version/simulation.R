library("lmtest")
library("sandwich") 
library("tidyverse") 
library(MASS)
library(xtable)
library(scales)


n=1000
nsim=100


dat_sim=function(n,beta_real,sig,gamma_real){
  X=matrix(rnorm(n=n*p, mean =0, sd = 1),nrow=n) # small sd of X
  e=rnorm(n,sd = sig)                            # e with mean=0 and sd=1
  Y= X %*% beta_real+ X %*% gamma_real *e  
  Y2=X %*% beta_real+e
  return(list(X=X, Y=Y,Y2=Y2))
}


#######################################
var_est=function(beta_real,beta_initial = NULL,X,Y){
  
w_est <- function(gamma){
  W <- NULL
  for (i in 1:n){
    #new <- 1/((t(X[i,]^2)%*% gamma)) 
    new <- 1/((t(X[i,]^2)%*% gamma))
    W <- c(W,new)
  }
  W <- diag(W)
  return(W)
}

beta_est=function(gamma){
  W <- w_est(gamma)
  beta <- solve(t(X) %*% W%*%X) %*%t(X)%*% W %*%Y
  return(beta)
}

gamma_est=function(beta){
  X2 <- X^2
  gamma <- (solve(t(X2)%*% X2) %*% t(X2) %*% (Y-X%*% beta )^2) 
  return(gamma)
}
if (is.null(beta_initial)) {
  beta_initial <- coef(lm(Y~.-1,data=data.frame(X=X,Y=Y)) )
}
beta=beta_initial
diff1 <- diff2 <- 10
gamma=gamma_est(beta)

tol <- M <-  exp(-100)
allbeta <- allgamma <- c()
while ( diff1 > tol & M < 100) { #diff2 >tol & 
  old_beta <- beta 
  old_gamma <- gamma 
  beta <- beta_est(gamma)
  gamma=gamma_est(beta) 
  diff1=sum((beta-old_beta)^2)
  M <- M + 1
}

return(beta)

}

####
sim_varguid=function(beta_real,gamma_real,sig=sig,nsim){
  MSE1=MSE2=MSE3=NULL
for (i in 1:nsim) {
  sim=dat_sim(n,beta_real,sig = sig,gamma_real) 
  m1=lm(Y~.-1,data=data.frame(X=sim[[1]],Y=sim[[3]])) 
  MSE1[i]=mean((coef(m1)-beta_real)^2)
  
  m2=lm(Y~.-1,data=data.frame(X=sim[[1]],Y=sim[[2]])) 
  MSE2[i]=mean((coef(m2)-beta_real)^2)
  
  beta2=var_est(beta_real,X=sim[[1]],Y=sim[[2]])
  MSE3[i]=mean((beta2-beta_real)^2)
}
  return(list(MSE1=MSE1,MSE2=MSE2,MSE3=MSE3))
}


#########
p=5;p2=5
res5=sim_varguid(beta_real=rep(-1,p),gamma_real=rep(1,p),sig=1,nsim)
p=10;p2=10
res10=sim_varguid(beta_real=rep(-1,p),gamma_real=rep(1,p),sig=1,nsim)
p=20;p2=20
res20=sim_varguid(beta_real=rep(-1,p),gamma_real=rep(1,p),sig=1,nsim)
p=50;p2=50
res50=sim_varguid(beta_real=rep(-1,p),gamma_real=rep(1,p),sig=1,nsim)


par(mfrow = c(2, 2))
boxplot(res5[[1]],res5[[2]],res5[[3]],ylim=c(0,0.05),
        names=c("OLS in normal setting","OLS in gamma setting","varGuid"),ylab='MSE',main="p=5")
boxplot(res10[[1]],res10[[2]],res10[[3]],ylim=c(0,0.1),
        names=c("OLS in normal setting","OLS in gamma setting","varGuid"),ylab='MSE',main="p=10")
boxplot(res20[[1]],res20[[2]],res20[[3]],ylim=c(0,2),
        names=c("OLS in normal setting","OLS in gamma setting","varGuid"),ylab='MSE',main="p=20")
boxplot(res20[[1]],res50[[2]],res50[[3]],ylim=c(0,25),
        names=c("OLS in normal setting","OLS in gamma setting","varGuid"),ylab='MSE',main="p=50")
#title("Rea")
mtext("Real beta=-1, nsim=20, n=1000", side = 3, line = -2, outer = T)

MSE_table=rbind(colMeans(as.data.frame(res5)),colMeans(as.data.frame(res10)),
                colMeans(as.data.frame(res20)),colMeans(as.data.frame(res50)))

rownames(MSE_table)=c("p=5","p=10","p=20","p=50")
MSE_table

#### try p=1 to 9

## 03/20 Task
## (1) add the follwing no gamma scenario with OLS MSE and make a table
## Y= X %*% beta_real+ e  
## Adjust beta_real and gamma_real and what we want: OLS MSE for above 0.05 vs OLS MSE for gamma simulation 0.2
## (2) find the plot of grouped boxplot from Flashdata
## plan
## (1) lm() use fancy approaches for solve(t(X) %*% X) -- se Class 3 note in GLM --- Therefore, try to use lm() as possible as we can
## eg. is beta_est the same as  lm(Y~., weight = W)
## (2) calculate log-likehood goodness of fit test compared with OLS, if varGuid is not better we just give OLS solution
## (3) calculate SE or Var(beta_hat) like sandwich est but might not need for SE for varGuid, Class 6 note in GLM --- in the future, we have to check the coverage of CI, is 95%?

## 03/29 Task
## a) continue (3) from 03/20
## b) note: introduction and simulation description
## c) something to think about y_hat: is there anything else can make a difference on y_hat besides new beta_hat

