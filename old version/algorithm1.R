
###
p=5;n=1000;beta=rep(1,p);gamma=rep(0.3,p);sig=1# real value
p2=5

tol=exp(-10)
dat_sim=function(n,p,p2,beta,sig){
  X=matrix(rnorm(n=n*p, mean =0, sd = 1),nrow=n) # small sd of X
  e=rnorm(n,sd = sig)                            # e with mean=0 and sd=1
  Y= X %*% beta+ X%*% gamma *e  
  return(list(X=X, Y=Y))
}

data=dat_sim(n,p,p2,beta,sig = sig)
X=data[[1]]
Y=data[[2]]
df=data.frame(X,Y=Y)
######################################################## from Dr. Lu's pdf

w_est <- function(gamma){
  W <- NULL
  for (i in 1:n){
    #new <- 1/((t(X[i,]^2)%*% gamma)) 
    new <- 1/((t(X[i,]^2)%*% gamma^2))
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
  gamma <- (solve(t(X2)%*% X2) %*% t(X2) %*% (Y-X%*% beta )^2) /sig # Sibei: I add a /sigma
  return(gamma)
}
##### check if the above result is correct

beta_est(rep(0.3,p)) # input the real value of gamma, gives us the right real beta
gamma_est(rep(1,p)) # input the real value of beta, NOT !!! gives us the right real gamma

#gamma0=rep(0.2,p) # initial
beta=rep(0.5,p)    # initial guess
sum(abs(coef(lm(Y~.-1,data=df))-rep(1,5))) # the results from OLS
diff1 <- diff2 <- 10
# gamma=gamma_est(beta,sig=sig) #rep(0.3,5)#
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

beta
sum(abs(beta-rep(1,5)))
abs(gamma) 


