
source("varGuid20240406.R")
n=100
p=5

beta_real=rep(-1,p);sig =1;gamma_real=rep(1,p)

dat_sim=function(n,beta_real,sig,gamma_real){
  X=matrix(rnorm(n=n*p, mean =0, sd = 1),nrow=n) # small sd of X
  e=rnorm(n,sd = sig)                            # e with mean=0 and sd=1
  Y= X %*% beta_real+ X %*% gamma_real *e
  Y2=X %*% beta_real+e
  return(list(X=X, Y=Y,Y2=Y2))
}
data <- dat_sim(n,beta_real, sig, gamma_real)
X <- data[[1]]
Y <- data[[2]]
df <- data.frame(X, Y=Y)
o <- lmv(X, Y)
beta <- o$beta
summary(o$obj.OLS)
confint(o$obj.OLS)
for(i in 1:length(o$obj.varGuid.coef)){
  print(names(o$obj.varGuid.coef)[i])
  print(confint(o$obj.varGuid.coef[[i]]))
}




## not include intercept
sum(abs(beta[-1]-beta_real))
sum(abs(coef(lm(Y~.,data=df))[-1]-beta_real)) # the results from OLS

