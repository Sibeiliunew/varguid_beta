library("sandwich") 
library("tidyverse") 
library(MASS)
library(xtable)
library(scales)
library(caTools)
source("./20240412/VarGuid20240412.R")
source("./leash2.0.7.R")

n=1000
nsim=500

#### please load sim5.RDS to get all sim_dat
sim5=readRDS("sim5.RDS")


######### initial
p=5;p2=5
sim_5=est_beta=names=NULL

gamma_real=rep(1,p)
beta_real=c(-1,-0.5,0,0.5,1)
sig=1
X=matrix(rnorm(n=n*p, mean =0, sd = 1),nrow=n) # small sd of X

Y= X %*% beta_real+ X %*% gamma_real *rnorm(n,sd = sig)   # e with mean=0 and sd=1
res5=lmv(X, Y)

all_ci <- list()
all_ci <- vector(mode='list', p)

names=names(res5$obj.varGuid.coef)


for(d in 1:p){
#  beta_ci[[d]]=rbind(confint(res5$obj.OLS)[d+1,],confint(res5$obj.varGuid)[d+1,])
  est_beta[[d]]=res5$beta[d+1]
  for(i in 1:length(res5$obj.varGuid.coef)){
    all_ci[[d]]=rbind(all_ci[[d]],confint(res5$obj.varGuid.coef[[i]])[d+1,])
  }
  
}

sim_5[[1]]=list(X,Y)

#####  

for (i in 2:nsim){
  Y= X %*% beta_real+ X %*% gamma_real *rnorm(n,sd = sig)  
  res5=lmv(X, Y)
  new <- list()
  new= vector(mode='list', p)
  for(d in 1:p){
  for(f in 1:length(res5$obj.varGuid.coef)){
    new[[d]]=rbind(new[[d]],confint(res5$obj.varGuid.coef[[f]])[d+1,])
  }}
  # w: 1-5
  for (w in 1:p){
    all_ci[[w]]=rbind(all_ci[[w]],new[[w]])
    est_beta[[w]]=c(est_beta[[w]],res5$beta[w+1])
  }
  
  sim_5[[i]]=list(X=X,Y=Y)
  
}

########
ci_res=matrix(NA,nrow=p,ncol=length(names))
colnames(ci_res)=names
rownames(ci_res)=c("beta1", "beta2", "beta3","beta4","beta5")

OLS=Var=NULL
for (i in 1:p){
  beta5=as.data.frame(all_ci[[i]]) %>% janitor::clean_names()
  beta5$type=rep(names,nsim)
  beta5$sim=factor(rep(1:nsim, each=length(names)))
  
  for (d in 1:length(names)){
  type=beta5 %>% filter(type==names[d])
  ci_res[i,d]=1-length(union(which(type$x2_5_percent >beta_real[i]), which(type$x97_5_percent < beta_real[i])))/nsim}

  
}

ci_res 
# saveRDS(all_ci,"all_ci.RDS")
# saveRDS(ci_res,"cires.RDS")
# saveRDS(sim_5,"sim5.RDS")

# diff=NULL
# for ( i in 1:p){
#   diff[[i]]=est_beta[[i]]-beta_real[i]
# }

#par(mfrow = c(2, 3))
# for ( c in 1:p){
#   boxplot(diff[[c]])
#   abline(h = 0,lty = 2 )
#   title(sub =paste0( "Real beta =",beta_real[i]),x=paste0("beta",c),y="Difference")
# }

# for (i in 1:p){
#   print(mean(est_beta[[i]]))
# }

#dat=readRDS("sim5.RDS")
#X=as.data.frame(dat[[1]][1])
#Y=unlist(dat[[1]][2])
n=1000
nsim=500
p=5
gamma_real =rep(1,p)
beta_real=c(-1,-0.5,0,0.5,1)
sig=1
X=matrix(rnorm(n=n*p, mean =0, sd = 1),nrow=n) # small sd of X

  # e with mean=0 and sd=1

  MSE1=MSE2=list()
  MSE1=MSE2 =vector(mode='list', p)
  for (i in 1:nsim) {
    #m1=lm(Y~.,data=data.frame(X=sim[[1]],Y=sim[[3]]))  # OLS in normal setting
    #MSE1[i]=mean((coef(m1)[-1]-beta_real)^2)
    Y= X %*% beta_real+ X^2 %*% gamma_real *rnorm(n,sd = sig) 
    m2=lm(Y~.,data=data.frame(X=X,Y=Y)) # OLS 
    holder=(coef(m2)[-1]-beta_real)^2
    beta2=lmv(X=X,Y=Y)$beta
    holder2=(beta2[-1]-beta_real)^2
    for(d in 1:p){
      MSE1[[d]]=c(MSE1[[d]],holder[d])
      MSE2[[d]]=c(MSE2[[d]],holder2[d])
    }
  }
  
  MSE1=as.data.frame(do.call(cbind,MSE1))
  MSE2=as.data.frame(do.call(cbind,MSE2))
  MSE1$type=rep("OLS",nrow(MSE1))
  MSE2$type=rep("Varguid",nrow(MSE2))
  res=rbind(MSE1,MSE2)

  colnames(res)=c("beta1","beta2","beta3","beta4","beta5")
  colMeans(res[1:500,1:5])
  colMeans(res[500:1000,1:5])



########


### for the catscore the fev1 
### test the prediction
#
# ols=lm(Y~.,dat=train[,-7])
# 
# msetrain_ols=mean(ols$residuals^2) 
# yhat_ols=fitted(ols)
# msetest_ols=mean((test$Y- yhat_ols)^2) 
  n=1000
  nsim=500
  p=5
  gamma_real =rep(1,p)
  beta_real=c(-1,-0.5,0,0.5,1)
  sig=1
X=matrix(rnorm(n=n*p, mean =0, sd = 1),nrow=n) # small sd of X

Y= X^2 %*% beta_real+ (X+1)^2 %*% gamma_real +rnorm(n,sd = sig)

dat=as.data.frame(cbind(X,Y))
dat$id=seq(1,nrow(dat),length.out=nrow(dat))

sample <- sample.split(dat$id, SplitRatio = 0.75)
train  <- subset(dat, sample == TRUE)
test   <- subset(dat, sample == FALSE)
train.x=train[,1:p]
train.y=train$V6
test.x=test[,1:p]
test.y=test$V6


o=lmv(X=apply(train.x,2,as.numeric),Y=train.y)

y.obj<- ymodv(o,gamma = c(seq(0,8.56, length.out=5)), phi = 0.45)

yhat.varGuid <- fnpred(mod=y.obj,
                       lmvo = o,
                       newdata = o$obj.varGuid$model[1:5,2:(ncol(o$obj.varGuid$model)-1)]) 


### test error
pred <- fnpred(mod=y.obj,lmvo = o,newdata = test.x)
rmse <- c()
rmse <- rbind(rmse,sqrt(colMeans((matrix(rep(test.y,ncol(pred)),length(test.y))-pred)^2)) )
### test error
rmse
colMeans(as.data.frame(rmse))
rmse <- as.list(as.data.frame(rmse))


