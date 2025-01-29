source("./20240425/simulation/generate_function_simulation.R")
source("./leash2.0.6.R")
source("./VarGuid20240626.R")
library(glmnet)
library(tidyverse)
library(MASS)
library(xtable)
library(scales)
library(faux)
library(olsrr)


nsim <- 1000

dat_sim=function(n,p,beta_real,gamma_real,corrv){
  
  if(p==1){
    X <- rnorm(mean=0,n=n)
  }else{
  
    X <- as.matrix(rnorm_multi(n = n, 
                     mu = rep(0,p),
                     sd = rep(1,p),
                     r = corrv, 
                     #varnames = letters[1:p],
                     empirical = FALSE))}
     e=rnorm(n,sd = 1)     
     if ( ! is.null(gamma_real)){
     Y= as.matrix(X) %*% beta_real+ (1+X %*% gamma_real) *e  
     
     return(list(X=X, Y=Y))}
  else{ Y2= as.matrix(X) %*% beta_real+ e 
        return(list(X=X,Y=Y2))
  }
  
  }

sim_varguid=function(n,p,beta_real,gamma_real,corrv,name){
  beta_var=beta_ols=vector(mode='list', p)
  dat=vector(mode='list', nsim)
  heter_test_p=NULL
  heter_test_sta=NULL
  for (i in 1:nsim) {
    sim=dat_sim(n,p,beta_real,gamma_real,corrv)
    
    dat[[i]]=cbind(sim[[1]],sim[[2]])
    
    m2=lm(Y~.,data=data.frame(X=sim[[1]],Y=sim[[2]])) # OLS 
    heter_test_holder_p=c(ols_test_breusch_pagan(m2)$p,ols_test_score(m2)$p,ols_test_f(m2)$p)
    heter_test_holder_sta=c(ols_test_breusch_pagan(m2)$bp,ols_test_score(m2)$score,ols_test_f(m2)$f)
    heter_test_p=rbind(heter_test_p,heter_test_holder_p)
    heter_test_sta=rbind(heter_test_sta,heter_test_holder_sta)
    holder=coef(m2)[-1]
    
    v2=lmv(X=sim[[1]],Y=sim[[2]])
    beta2=v2$beta
    ci=confint(v2$obj.varGuid.coef$HC3) # HC3 method used
    holder2=beta2[-1]
    
     for(d in 1:p){
      beta_ols[[d]]=c(beta_ols[[d]],holder[d])
      beta_var[[d]]=c(beta_var[[d]],holder2[d])
     }
    
  }
  saveRDS(dat,paste(eval(name),"_",eval(parse(text=n)),"with",eval(parse(text=p)),"with",eval(parse(text=corrv)),".rds",sep=""))
  
  beta_ols=as.data.frame(do.call(cbind,beta_ols))
  beta_var=as.data.frame(do.call(cbind,beta_var))
  beta_ols$type=rep("OLS",nrow(beta_ols))
  beta_var$type=rep("Varguid",nrow(beta_var))
  res=rbind(beta_ols,beta_var)
  colnames(heter_test_p)=c("Breusch Pagan","Score test","F test")
  colnames(heter_test_sta)=c("Breusch Pagan","Score test","F test")
  

  return(list(res=res,heter_test_p=heter_test_p,heter_test_sta=heter_test_sta))
}

#######################################################
##########
##########scenario2
#########################################################

## n=20 ,p=10, cor=0

sce2_1=sim_varguid(n=20,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=c(rep(1,5),rep(0,5)),corrv=0,name="sce2") 

apply(sce2_1$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce2_1$heter_test_sta,2,mean ),3)

## n=20 ,p=10, cor=0.9
sce2_2=sim_varguid(n=20,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=c(rep(1,5),rep(0,5)),corrv=0.9,name="sce2") 

apply(sce2_2$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce2_2$heter_test_sta,2,mean ),3)
### n=200, p=10, cov=0
sce2_3=sim_varguid(n=200,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=c(rep(1,5),rep(0,5)),corrv=0,name="sce2") 

apply(sce2_3$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce2_3$heter_test_sta,2,mean ),3)
### n=200, p=10, cov=0.9
sce2_4=sim_varguid(n=200,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=c(rep(1,5),rep(0,5)),corrv=0.9,name="sce2") 

apply(sce2_4$heter_test_p,2,function(x)length(which(x<0.05))/nsim )
round(apply(sce2_4$heter_test_sta,2,mean ),3)
#######################################################
##########
##########scenario4
#########################################################

sce4_1=sim_varguid(n=20,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=c(rep(0,5),rep(1,5)),corrv=0,name="sce4") 



apply(sce4_1$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce4_1$heter_test_sta,2,mean ),3)
#######
sce4_2=sim_varguid(n=20,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=c(rep(0,5),rep(1,5)),corrv=0.9,name="sce4") 

apply(sce4_2$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce4_2$heter_test_sta,2,mean ),3)

#####
sce4_3=sim_varguid(n=200,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=c(rep(0,5),rep(1,5)),corrv=0,name="sce4") 



apply(sce4_3$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce4_3$heter_test_sta,2,mean ),3)
#######
sce4_4=sim_varguid(n=200,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=c(rep(0,5),rep(1,5)),corrv=0.9,name="sce4") 

apply(sce4_4$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce4_4$heter_test_sta,2,mean ),3)

#######################################################
##########
##########scenario5 p=15
#########################################################

sce5_1=sim_varguid(n=20,p=15,beta_real=c(rep(1,5),rep(0,10)),
                   gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,5)),corrv=0,name="sce5") 

apply(sce5_1$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce5_1$heter_test_sta,2,mean ),3)
#####
sce5_2=sim_varguid(n=20,p=15,beta_real=c(rep(1,5),rep(0,10)),
                   gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,5)),corrv=0.9,name="sce5") 

apply(sce5_2$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce5_2$heter_test_sta,2,mean ),3)
#####
sce5_3=sim_varguid(n=200,p=15,beta_real=c(rep(1,5),rep(0,10)),
                   gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,5)),corrv=0,name="sce5") 

apply(sce5_3$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce5_3$heter_test_sta,2,mean ),3)
#####

sce5_4=sim_varguid(n=200,p=15,beta_real=c(rep(1,5),rep(0,10)),
                   gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,5)),corrv=0.9,name="sce5") 

apply(sce5_4$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce5_4$heter_test_sta,2,mean ),3)


########################################################
########## sce1
####################################################

sce1_1=sim_varguid(n=20,p=1,beta_real=1,
                   gamma_real=1,corrv=0,name="sce1") 

apply(sce1_1$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce1_1$heter_test_sta,2,mean ),3)


sce1_3=sim_varguid(n=200,p=1,beta_real=1,
                   gamma_real=1,corrv=0,name="sce1") 

apply(sce1_3$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce1_3$heter_test_sta,2,mean ),3)

########################################################
########## sce3
####################################################

#####
sce3_1=sim_varguid(n=20,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=NULL,corrv=0,name="sce3") 

apply(sce3_1$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce3_1$heter_test_sta,2,mean ),3)
#######
sce3_2=sim_varguid(n=20,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=NULL,corrv=0.9,name="sce3") 

apply(sce3_2$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce3_2$heter_test_sta,2,mean ),3)

######
sce3_3=sim_varguid(n=200,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=NULL,corrv=0,name="sce3") 
round(apply(sce3_3$heter_test_sta,2,mean ),3)
apply(sce3_3$heter_test_p,2,function(x) length(which(x<0.05))/nsim )

#######
sce3_4=sim_varguid(n=200,p=10,beta_real=c(rep(1,5),rep(0,5)),
                   gamma_real=NULL,corrv=0.9,name="sce3") 

apply(sce3_4$heter_test_p,2,function(x) length(which(x<0.05))/nsim )
round(apply(sce3_4$heter_test_sta,2,mean ),3)

