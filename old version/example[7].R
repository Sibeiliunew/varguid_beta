source("leash2.1.R")
source("varGuid20240418.R")
n=100
p=200

beta_real=c(rep(-1,5),rep(0,p-5));sig =1;gamma_real=c(rep(1,5),rep(0,p-5))

dat_sim=function(n,beta_real,sig,gamma_real){
  X=matrix(rnorm(n=n*p, mean =0, sd = 1),nrow=n) # small sd of X
  colnames(X) = paste("X", 1:p, sep ="")
  e=rnorm(n,sd = sig)                            # e with mean=0 and sd=1
  Y= X %*% beta_real+ X %*% gamma_real *e
  Y2=X %*% beta_real+e
  return(list(X=X, Y=Y,Y2=Y2))
}
data <- dat_sim(n,beta_real, sig, gamma_real)
X <- data[[1]]
Y <- data[[2]]
df <- data.frame(X, Y=Y)
o <- lmv(X, Y, lasso = TRUE)

beta <- o$beta

## the following code only available when lasso = FALSE
#summary(o$obj.OLS) 
#confint(o$obj.OLS)

### the .2p ones are deleted since they are not working
#for(i in 1:length(o$obj.varGuid.coef)){
#  print(names(o$obj.varGuid.coef)[i])
#  print(confint(o$obj.varGuid.coef[[i]]))
#}

## not include intercept
sum(abs(beta[-1]-beta_real))


###################### yhat prediction

y.obj<- ymodv(obj = o,gamma = c(seq(0,9, length.out=5)), phi = 0.46)

yhat.varGuid <- fnpred(mod=y.obj,
                       lmvo = o,
                       newdata = X[1:5,])

### there are 5 types of prediction from varGuid, we have to test which one is better (yhatOLS is the simple OLS yhat)
yhat.varGuid

