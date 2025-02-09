
source("./leash2.0.7.R")
source("./VarGuid20250206.R")
#source("./organized code:data/functions.R")
library(glmnet)
library(tidyverse)
library(MASS)
library(xtable)
library(scales)
library(faux)
library(olsrr)
library(caTools)
library(caret)
library(pheatmap)

genes=load("~/Documents/Dissertation/varguid/PAM50.RData")

##### p=50
  rmse=NULL
  real= as.data.frame(cbind(genes,outcome)) %>% drop_na(outcome)
  #real= as.data.frame(cbind(topgene,outcome)) %>% drop_na(outcome)
  #real = na.omit(real)
  real <- cbind(makeX(real[,1:(ncol(real)-1)]), real[,ncol(real)])
  real=apply(real,2,as.numeric)
  folds=createFolds(1:nrow(real), k = 10) ## 10 cv
  for (i in 1:10){
    print(i) 
    #trn <- sample.split(1:nrow(real), SplitRatio = 0.75)
    
    train  <- real[-folds[[i]],]
    test   <- real[folds[[i]],]
    
    data=list(x.train = train[,1:(ncol(real)-1)],
              y.train = train[,ncol(real)],
              x.test = test[,1:(ncol(real)-1)],
              y.test = test[,ncol(real)])
    
    o <- lmv(X =as.matrix(data$x.train) , Y = unlist(data$y.train), lasso = TRUE) # , lasso = TRUE
    y.obj <-ymodv(o,gamma = c(seq(0,9, length.out=5)), phi = 0.46)#, rf = FALSE)
    
    
    pred <- fnpred(mod=y.obj,lmvo = o,newdata = data$x.test)
    
    rmse <- rbind(rmse,sqrt(colMeans((matrix(rep(data$y.test,ncol(pred)),length(data$y.test))-pred)^2)) )
    
  }
  #rmse_res[[d]]=colMeans(as.data.frame(rmse))
  
  
  table1=colMeans(as.data.frame(rmse))
  table1

######## PAM50 number of overlapped selected genes
#######
    data=list(x = makeX(as.data.frame(real[,1:(ncol(real)-1)])),
              y= real[,ncol(real)])
    ### Varguid Lasso
    set.seed(2024)
    o1 <- lmv(X =as.matrix(data$x) , Y = unlist(data$y), lasso = TRUE)
    m=as.data.frame(as.matrix(o1$beta)) %>% filter(s0>0)
    n1=nrow(m)
    select1=rownames(m) # 
    ### Lasso
    o2 <- cv.glmnet(x =as.matrix(data$x) , y = data$y,alpha = 1,lambda =exp(seq(-10,10,length=200) ))
    m2=as.data.frame(as.matrix(coef(o2, s = "lambda.min"))) %>% filter(s1>0)
    n2=nrow(m2)
    select2=rownames(m2) 
    
    n_ol=length(intersect(select1,select2))
    n_ol
  
