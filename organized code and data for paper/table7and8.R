source("./20240425/simulation/generate_function_simulation.R")
source("./VarGuid20250206.R")
source("./leash2.0.7.R")
library(glmnet)
library(tidyverse)
library(caret)
library(caTools)
####################################################################
###   
###  Simulation data: simulate 10 datasets from the generate_function_simulation.R
### there are 12 simulations there: we don't use "lm" and "lmi"
###   n = 500, d = 15 for low p case
###   n = 100, d = 200 for high p case
####################################################################
## data settings for low p. lasso = FALSE



simnames <- c("cobra2",    "cobra8",    "friedman1", "friedman3", "inx1",      
              "inx2", "inx3",     # "lm",        "lmi",       
              "lmi2",      
              "sup",       "sup2")   

# you can use a number or a name to generate a specific simulation
#simo <- simulation.sum[[1]](n=n, d=d, corrv = corrv)
## another way
#simo <- simulation.sum[[simnames[i]]](n=n, d=d, corrv = corrv)$dta
n <- 500
d <- 15
corrv <- c(0, .9)[1] ## change
rmse3 <- c()
rmse_res3=NULL

for (c in 1:10){
  for( i in 1:50){
  print(i)
  simo <- simulation.sum[[simnames[c]]](n=n, d=d, corrv = corrv)$dta
  trn <- sample.split(1:n, SplitRatio = 0.75)
  train  <- subset(simo, trn==TRUE)
  test   <- subset(simo, trn==FALSE)
  
  data=list(x.train = makeX(train[,1:(ncol(simo)-1)]),
            y.train = train[,ncol(simo)],
            x.test = makeX(test[,1:(ncol(simo)-1)]),
            y.test = test[,ncol(simo)])
  
  o <- lmv(X =as.matrix(data$x.train) , Y = unlist(data$y.train), lasso = FALSE) # , 
  y.obj <-ymodv(o,gamma = c(seq(0,9, length.out=5)), phi = 0.46)#, rf = FALSE)
  
  
  pred <- fnpred(mod=y.obj,lmvo = o,newdata = data$x.test)
  
  rmse3 <- rbind(rmse3,sqrt(colMeans((matrix(rep(data$y.test,ncol(pred)),length(data$y.test))-pred)^2)) )
  }
  rmse_res3[[c]]=colMeans(as.data.frame(rmse3))
}

table3=do.call("rbind",rmse_res3)
rownames(table3)=simnames
t3=round(table3,5)

table3_3=do.call("rbind",rmse_res3)
rownames(table3_3)=simnames
t3_3=round(table3_3,3)

#######
##### for table 8: lasso and var-lasso overlap


n <- 100
d <- 200
corrv <- c(0, .9)[1] ## change
rmse_res1=NULL
rmse_res2=NULL
simnames2=c("cobra2", "inx2", "inx3","lmi2" )
simnames3=c("cobra2", "inx3","lmi2","sup" )
######
for (c in 1:4){
  set.seed(2024)
  simo <- simulation.sum[[simnames3[c]]](n=n, d=d, corrv = corrv)$dta
    print(c)
    data=list(x = makeX(simo[,1:(ncol(simo)-1)]),
              y= simo[,ncol(simo)])
    ### Varguild Lasso
    set.seed(2024)
    o1 <- lmv(X =as.matrix(data$x) , Y = unlist(data$y), lasso = TRUE)
    m=as.data.frame(as.matrix(o1$beta)) %>% filter(s0>0)
    n1=nrow(m)
    select1=rownames(m) # 
    ### Lasso
    set.seed(2024)
    o2 <- cv.glmnet(x =as.matrix(data$x) , y = data$y,alpha = 1, maxit = 5000)
    m2=as.data.frame(as.matrix(coef(o2, s = "lambda.min"))) %>% filter(s1>0)
    n2=nrow(m2)
    select1=rownames(m2) 
    
    n_ol=length(intersect(select1,select2))
    
  rmse_res2[[c]]=c(n1,n2,n_ol)
}

table1=do.call("rbind",rmse_res1)
rownames(table1)=simnames2
table2=do.call("rbind",rmse_res2)
rownames(table2)=simnames3


######RMSE
##### larger p lasso = TRUE
simnames <- c(#"cobra2" )     
              "lmi2",      
              "sup")   

n <- 100
d <- 200
corrv <- c(0, .9)[2] ## change
rmse <- c()
rmse_res4=NULL

for (c in 1:length(simnames)){
  for( i in 1:50){
    print(i)
    simo <- simulation.sum[[simnames[c]]](n=n, d=d, corrv = corrv)$dta
    trn <- sample.split(1:n, SplitRatio = 0.75)
    train  <- subset(simo, trn==TRUE)
    test   <- subset(simo, trn==FALSE)
    data=list(x.train = makeX(train[,1:(ncol(simo)-1)]),
              y.train = train[,ncol(simo)],
              x.test = makeX(test[,1:(ncol(simo)-1)]),
              y.test = test[,ncol(simo)])
    
    o <- lmv(X =as.matrix(data$x.train) , Y = unlist(data$y.train), lasso = TRUE) # , lasso = TRUE
    y.obj <-ymodv(obj = o,gamma = c(seq(0,9, length.out=5)), phi = 0.46)#, rf = FALSE)
    
    
    pred <- fnpred(mod=y.obj,lmvo = o,newdata = data$x.test)
    
    rmse4 <- rbind(rmse,sqrt(colMeans((matrix(rep(data$y.test,ncol(pred)),length(data$y.test))-pred)^2)) )
  }
  rmse_res4[[c]]=colMeans(as.data.frame(rmse4))
}

table4=do.call("rbind",rmse_res4)
rownames(table4)=simnames
t4=round(table4,5)


table4_4=do.call("rbind",rmse_res4)
rownames(table4_4)=simnames
t4_4=round(table4_4,5)




rmse <- c()
rmse_res4=NULL

for (c in 1:10){
  for( i in 1:50){
    print(i)
    simo <- simulation.sum[[simnames[c]]](n=n, d=d, corrv = corrv)$dta
    trn <- sample.split(1:n, SplitRatio = 0.75)
    train  <- subset(simo, trn==TRUE)
    test   <- subset(simo, trn==FALSE)
    data=list(x.train = makeX(train[,1:(ncol(simo)-1)]),
              y.train = train[,ncol(simo)],
              x.test = makeX(test[,1:(ncol(simo)-1)]),
              y.test = test[,ncol(simo)])
    
    o <- lmv(X =as.matrix(data$x.train) , Y = unlist(data$y.train), lasso = TRUE) # , lasso = TRUE
    y.obj <-ymodv(obj = o,gamma = c(seq(0,9, length.out=5)), phi = 0.46)#, rf = FALSE)
    
    
    pred <- fnpred(mod=y.obj,lmvo = o,newdata = data$x.test)
    
    rmse4 <- rbind(rmse,sqrt(colMeans((matrix(rep(data$y.test,ncol(pred)),length(data$y.test))-pred)^2)) )
  }
  rmse_res4[[c]]=colMeans(as.data.frame(rmse4))
}

table4=do.call("rbind",rmse_res4)
table4


#### for larger p real data
#####

library("datamicroarray")
data.names <- c("alon","christensen","gravier","pomeroy","shipp", "singh","tian", "west" ,"gordon","subramanian")


## use lasso to find the outcome: the continuous outcome will be the most important gene for 
## predicting the classification outcome

library(glmnet)
for (i in 3:length(data.names)){
nm <- data.names[i]
data(list=nm, package = 'datamicroarray')
x <- eval(parse(text=nm))$x
y <- eval(parse(text=nm))$y

#dat=cbind(x,y) %>%as.data.frame() %>%  filter(y != "placenta") %>% mutate(y=factor(y,levels=c("blood","other")))
#x=dat[,1:(ncol(dat)-1)]
#y=dat[,ncol(dat)]

x <- scale(x)
x <- makeX(as.data.frame(x))
cv_model <- cv.glmnet(x,y , family = "binomial") # binomial

best_lambda <- cv_model$lambda.min
#print(best_lambda)
o <- glmnet(x,y , alpha = 1, lambda = best_lambda, family = "binomial")
yindex <- which.max(abs(as.vector(o$beta)))

print(colnames(x)[yindex])
}

outcomes=c("X765","OSM_P188_F","g1CNS26","D28473-s-at","V2006","V10234","898_s_at","V132","34320_at","BAX")
realDat=NULL
for (i in 1:length(data.names)){
  nm <- data.names[i]
  data(list=nm, package = 'datamicroarray')
  x <- eval(parse(text=nm))$x
  if(i == 8){realDat[[i]] <- cbind(x[,-132],x[,132])} else{
  index=which(colnames(x)==outcomes[i])
  x <- scale(x)
  realDat[[i]] <- cbind(x[,-index],x[,index])}
}



##############
overlap_res=NULL
for (c in 1:10){
  real <- realDat[[c]]
    print(c)
    #trn <- sample.split(1:nrow(real), SplitRatio = 0.75)
    data=list(x = makeX(as.data.frame(real[,1:(ncol(real)-1)])),
              y= real[,ncol(real)])
    ### Varguild Lasso
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
    
  overlap_res[[c]]=c(n1,n2,n_ol)
}

table4=do.call("rbind",overlap_res)

####### RMSE
#corrv <- c(0, .9)[1]
#rmse4 <- c()
#rmse_res4=NULL

for (c in 1:10){
  real <- realDat[[c]]
  folds=createFolds(1:nrow(real), k = 10)
  for( i in 1:10){
    print(i)
    #trn <- sample.split(1:nrow(real), SplitRatio = 0.75)
    train  <- real[-folds[[i]],]
    test   <- real[folds[[i]],]
    data=list(x.train = makeX(train[,1:(ncol(real)-1)]),
              y.train = train[,ncol(real)],
              x.test = makeX(test[,1:(ncol(real)-1)]),
              y.test = test[,ncol(real)])
    
    o <- lmv(X =as.matrix(data$x.train) , Y = unlist(data$y.train), lasso = TRUE) # , lasso = TRUE
    y.obj <-ymodv(o,gamma = c(seq(0,9, length.out=5)), phi = 0.46)#, rf = FALSE)
    
    
    pred <- fnpred(mod=y.obj,lmvo = o,newdata = data$x.test)
    
    rmse4 <- rbind(rmse,sqrt(colMeans((matrix(rep(data$y.test,ncol(pred)),length(data$y.test))-pred)^2)) )
  }
  rmse_res4[[c]]=colMeans(as.data.frame(rmse4))
}

table4=do.call("rbind",rmse_res4)
table4



