source("./20240412/leash2.0.R") # change path
source("./20240412/varGuid20240412.R")
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
### the .2p ones are deleted since they are not working
for(i in 1:length(o$obj.varGuid.coef)){
  print(names(o$obj.varGuid.coef)[i])
  print(confint(o$obj.varGuid.coef[[i]]))
}

## not include intercept
sum(abs(beta[-1]-beta_real))
sum(abs(coef(lm(Y~.,data=df))[-1]-beta_real)) # the results from OLS

###################### yhat prediction
y.obj<- ymodv(o,gamma = c(seq(0,8.56, length.out=5)), phi = 0.45)

yhat.varGuid <- fnpred(mod=y.obj,
                       lmvo = o,
                       newdata = o$obj.varGuid$model[1:5,2:6])

### there are 5 types of prediction from varGuid, we have to test which one is better (yhatOLS is the simple OLS yhat)
yhat.varGuid


