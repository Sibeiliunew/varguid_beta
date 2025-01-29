
source("./leash2.0.6.R")
source("./VarGuid20240626.R")
library(glmnet)
library(tidyverse)
library(MASS)
library(xtable)
library(scales)
library(faux)
library(olsrr)


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

######################################################################
#######################################################################
######################################################################


sim_varguid=function(n,p,beta_real,gamma_real,corrv,name,lasso_status=FALSE){
  
  if(lasso_status==TRUE){
    beta_var=beta_lasso=vector(mode='list', p)
    dat=vector(mode='list', nsim)
    for (i in 1:nsim) {
      sim=dat_sim(n,p,beta_real,gamma_real,corrv) 
      dat[[i]]=cbind(sim[[1]],sim[[2]])
      
      
      v2=lmv(X=sim[[1]],Y=sim[[2]],lasso = TRUE)
      beta2=v2$beta
      holder2=beta2[-1]
      holder=coef(v2$obj.lasso)[-1]
      
      for(d in 1:p){
        beta_lasso[[d]]=c(beta_lasso[[d]],holder[d])
        beta_var[[d]]=c(beta_var[[d]],holder2[d])
      }
    }
 #   saveRDS(dat,paste(eval(name),"_",eval(parse(text=n)),"with",eval(parse(text=p)),"with",eval(parse(text=corrv)),".rds",sep=""))
    beta_lasso=as.data.frame(do.call(cbind,beta_lasso))
    beta_var=as.data.frame(do.call(cbind,beta_var))
    beta_lasso$type=rep("LASSO",nrow(beta_lasso))
    beta_var$type=rep("Varguid_lasso",nrow(beta_var))
    res=rbind(beta_lasso,beta_var)
    return(list(res=res))
  }else{
    beta_var=beta_ols=vector(mode='list', p)
    dat=vector(mode='list', nsim)
    heter_test_p=NULL
    heter_test_sta=NULL
    for (i in 1:nsim) {
      sim=dat_sim(n,p,beta_real,gamma_real,corrv)
      
      dat[[i]]=cbind(sim[[1]],sim[[2]])
      
      m2=lm(Y~.,data=data.frame(X=sim[[1]],Y=sim[[2]]),lasso=lasso_status) # OLS 
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
    
  beta_ols=as.data.frame(do.call(cbind,beta_ols))
  beta_var=as.data.frame(do.call(cbind,beta_var))
  beta_ols$type=rep("OLS",nrow(beta_ols))
  beta_var$type=rep("Varguid",nrow(beta_var))
  res=rbind(beta_ols,beta_var)
  colnames(heter_test_p)=c("Breusch Pagan","Score test","F test")
  colnames(heter_test_sta)=c("Breusch Pagan","Score test","F test")
  
  return(list(res=res,heter_test_p=heter_test_p,heter_test_sta=heter_test_sta))
  }

}
######################################################################
#######################################################################
######################################################################

varguid=function(dat,lasso_status){
  p=ncol(dat[[1]])-1
  beta_var=beta_lasso=vector(mode='list', p)
  for (i in 1:nsim) {
    X=dat[[i]][,1:p]
    Y=dat[[i]][,p+1]
    
    m2=lm(Y~.,data=data.frame(X=X,Y=Y)) # OLS 
    holder=coef(m2)[-1]
    
    v2=lmv(X=X,Y=Y,lasso=lasso_status)
    beta2=v2$beta
    holder2=beta2[-1]
    for(d in 1:p){
      beta_lasso[[d]]=c(beta_lasso[[d]],holder[d])
      beta_var[[d]]=c(beta_var[[d]],holder2[d])
    }
  }
  beta_lasso=as.data.frame(do.call(cbind,beta_lasso))
  beta_var=as.data.frame(do.call(cbind,beta_var))
  beta_lasso$type=rep("Lasso",nrow(beta_lasso))
  beta_var$type=rep("Varguid",nrow(beta_var))
  res=rbind(beta_lasso,beta_var)
  return(list(res=res))
}

MSE1=function(b, true=1){
  return( mean((b-true)^2) )
}

MSE0=function(b, true=0){
  return( mean((b-true)^2) )
}

bias=function(b,true=1){
  return( mean(b)-true)
}

cover=function(ci,real){
  r1= ci[,1]<real
  r2=ci[,2]>real
  res_ci=NULL
  for (i in 1:length(r1)){
    if(r1[i]== T & r2[i]== T){new=TRUE}
    else{new=FALSE }
    res_ci=c(res_ci,new)}
  return(res_ci)
}


ci_organize=function(dat,beta_real){
  MLE_ci_1=MLE_ci_0=HC3_ci_1=HC3_ci_0=HC0_ci_1=HC0_ci_0=OLSHC3_ci_1=OLSHC0_ci_1=NULL
  OLSHC0_ci_0=OLSHC3_ci_0=NULL
  
  for ( i in 1:nsim ){
    X=as.matrix(dat[[i]][,1:(ncol(dat[[i]])-1)])
    Y=as.matrix(dat[[i]][,ncol(dat[[i]])])
    #res6=lm(Y~.,data=data.frame(X=X,Y=Y)) # OLS
    res5=lmv(X, Y)
    ### varguid
    if(ncol(X)==1){
      
      OLS=confint(coeftest(res5$obj.OLS, vcov = vcovHC(res5$obj.OLS, "HC3")))[which(beta_real==1)+1,]
      OLSHC3_ci_1=rbind(OLSHC3_ci_1,OLS)
      OLS2=confint(coeftest(res5$obj.OLS, vcov = vcovHC(res5$obj.OLS, "HC0")))[which(beta_real==1)+1,]
      OLSHC0_ci_1=rbind(OLSHC0_ci_1,OLS2)
      
      MLE_ci_1=rbind(MLE_ci_1,confint(res5$obj.varGuid.coef[[1]])[which(beta_real==1)+1,])
      HC3_ci_1=rbind(HC3_ci_1,confint(res5$obj.varGuid.coef[[2]])[which(beta_real==1)+1,])
      HC0_ci_1=rbind(HC0_ci_1,confint(res5$obj.varGuid.coef[[4]])[which(beta_real==1)+1,])
    }else{
      
      OLS=confint(coeftest(res5$obj.OLS, vcov = vcovHC(res5$obj.OLS, "HC3")))[which(beta_real==1)+1,]
      OLSHC3_ci_1=rbind(OLSHC3_ci_1,OLS)
      OLS2=confint(coeftest(res5$obj.OLS, vcov = vcovHC(res5$obj.OLS, "HC0")))[which(beta_real==1)+1,]
      OLSHC0_ci_1=rbind(OLSHC0_ci_1,OLS2)
      
      MLE_ci_1=rbind(MLE_ci_1,confint(res5$obj.varGuid.coef[[1]])[which(beta_real==1)+1,])
      HC3_ci_1=rbind(HC3_ci_1,confint(res5$obj.varGuid.coef[[2]])[which(beta_real==1)+1,])
      HC0_ci_1=rbind(HC0_ci_1,confint(res5$obj.varGuid.coef[[4]])[which(beta_real==1)+1,])
      MLE_ci_0=rbind(MLE_ci_0,confint(res5$obj.varGuid.coef[[1]])[which(beta_real==0)+1,])
      HC3_ci_0=rbind(HC3_ci_0,confint(res5$obj.varGuid.coef[[2]])[which(beta_real==0)+1,])
      HC0_ci_0=rbind(HC0_ci_0,confint(res5$obj.varGuid.coef[[4]])[which(beta_real==0)+1,])
      OLS3=confint(coeftest(res5$obj.OLS, vcov = vcovHC(res5$obj.OLS, "HC3")))[which(beta_real==0)+1,]
      OLSHC3_ci_0=rbind(OLSHC3_ci_0,OLS3)
      OLS4=confint(coeftest(res5$obj.OLS, vcov = vcovHC(res5$obj.OLS, "HC0")))[which(beta_real==0)+1,]
      OLSHC0_ci_0=rbind(OLSHC0_ci_0,OLS4)
    }
  }
  
  if(ncol(X)==1){
    MLE_1=sum(cover(MLE_ci_1,real=1))/(length(which(beta_real==1))*nsim)
    HC3_1=sum(cover(HC3_ci_1,real=1))/(length(which(beta_real==1))*nsim)
    HC0_1=sum(cover(HC0_ci_1,real=1))/(length(which(beta_real==1))*nsim)
    
    OLSHC3_1=sum(cover(OLSHC3_ci_1,real=1))/(length(which(beta_real==1))*nsim)
    OLSHC0_1=sum(cover(OLSHC0_ci_1,real=1))/(length(which(beta_real==1))*nsim)
    
    beta_1=data.frame(OLS_HC3=OLSHC3_1,OLS_HC0=OLSHC0_1,
                      MLE=MLE_1,HC3=HC3_1,HC0=HC0_1)
    return(list(beta_1=beta_1))}
  else{
    MLE_1=sum(cover(MLE_ci_1,real=1))/(length(which(beta_real==1))*nsim)
    MLE_0=sum(cover(MLE_ci_0,real=0))/(length(which(beta_real==0))*nsim)
    HC3_1=sum(cover(HC3_ci_1,real=1))/(length(which(beta_real==1))*nsim)
    HC3_0=sum(cover(HC3_ci_0,real=0))/(length(which(beta_real==0))*nsim)
    HC0_1=sum(cover(HC0_ci_1,real=1))/(length(which(beta_real==1))*nsim)
    HC0_0=sum(cover(HC0_ci_0,real=0))/(length(which(beta_real==0))*nsim)
    
    #####
    OLSHC3_1=sum(cover(OLSHC3_ci_1,real=1))/(length(which(beta_real==1))*nsim)
    OLSHC0_1=sum(cover(OLSHC0_ci_1,real=1))/(length(which(beta_real==1))*nsim)
    OLSHC3_0=sum(cover(OLSHC3_ci_0,real=0))/(length(which(beta_real==0))*nsim)
    OLSHC0_0=sum(cover(OLSHC0_ci_0,real=0))/(length(which(beta_real==0))*nsim)
    
    
    beta_1=data.frame(OLS_HC3=OLSHC3_1,OLS_HC0=OLSHC0_1,
                      MLE=MLE_1,HC3=HC3_1,HC0=HC0_1)
    beta_0=data.frame(OLS_HC3=OLSHC3_0,OLS_HC0=OLSHC0_0,
                      MLE=MLE_0,HC3=HC3_0,HC0=HC0_0)
    return(list(beta_1=beta_1,beta_0=beta_0))}
  
}

yhat=function(dat,test,lasso_status){
  rmse3 <- c()
  rmse_res3=NULL
  nsim=length(dat)
  
  same_name=colnames(test[[1]])
  for( i in 1:nsim){ 
    dat_sub=as.data.frame(dat[[i]])
    colnames(dat_sub[,1:(ncol(dat_sub)-1)])=same_name
    data=list(x.train = makeX(as.data.frame(dat_sub[,1:(ncol(dat_sub)-1)])),
              y.train = as.matrix(dat_sub[,ncol(dat_sub)]),
              x.test = makeX(as.data.frame(test[[1]])),
              y.test = test[[2]])
    
    colnames(data$x.train) <- colnames(data$x.test)
    if(ncol(dat[[1]])==2){ o <- lmv(X =as.vector(data$x.train) , Y = as.vector(data$y.train), lasso = lasso_status)}else{
      o <- lmv(X =as.matrix(data$x.train) , Y = unlist(data$y.train), lasso = lasso_status)} # , lasso = TRUE
    
    y.obj <-   tryCatch({
      ymodv(o,gamma = c(seq(0,9, length.out=4)), phi = 0.46)#, rf = FALSE)
    }, error=function(e){cat("error happens",i,"run")}) 
    
    pred <- fnpred(mod=y.obj,lmvo = o,newdata = data$x.test)
    
    rmse3 <- rbind(rmse3,sqrt(colMeans((matrix(rep(data$y.test,ncol(pred)),length(data$y.test))-pred)^2)) )
  }
  
  
  return(colMeans(as.data.frame(rmse3[,1:ncol(rmse3)])) )# mean for the 1000 nsim
}



