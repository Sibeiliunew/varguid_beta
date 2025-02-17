source("./20240425/simulation/generate_function_simulation.R")
source("./leash2.0.9.R")
source("./VarGuid20250212.R")

source("./leash2.0.6.R")
source("./VarGuid20240626.R")
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
corrv <- c(0, .9)[2] ## change
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
  
  rmse3 <- rbind(rmse3,sqrt(colMeans((matrix(rep(data$y.test,ncol(pred)),length(data$y.test))-pred)^2,na.rm = T)) )
  }
  rmse_res3[[c]]=colMeans(as.data.frame(rmse3),na.rm = T)
}

table3=do.call("rbind",rmse_res3)
rownames(table3)=simnames
t3=round(table3,5)

table3_3=do.call("rbind",rmse_res3)
rownames(table3_3)=simnames
t3_3=round(table3_3,5)


######RMSE
##### larger p lasso = TRUE

#simnames=c("cobra2",    "cobra8",    "friedman1", "friedman3", "inx1",      
#           "inx2", "inx3",     # "lm",        "lmi",       
#           "lmi2",      
#           "sup",       "sup2") 
simnames=c("lmi2")


simnames=c("cobra2") 

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
    
    rmse4 <- rbind(rmse,sqrt(colMeans((matrix(rep(data$y.test,ncol(pred)),length(data$y.test))-pred)^2,na.rm = TRUE)) )
  }
  rmse_res4[[c]]=colMeans(as.data.frame(rmse4),na.rm = TRUE)
}

table4_6=do.call("rbind",rmse_res4)
rownames(table4_6)=simnames
t4_6=round(table4_6,5)

table4_5=do.call("rbind",rmse_res4)
rownames(table4_5)=simnames
t4_5=round(table4_5,5)

table4=do.call("rbind",rmse_res4)
rownames(table4)=simnames
t4=round(table4,5)


table4_4=do.call("rbind",rmse_res4)
rownames(table4_4)=simnames
t4_4=round(table4_4,5)

#######
##### for table 8: lasso and var-lasso overlap for simulated data
####larger p lasso = TRUE

n <- 100
d <- 200
corrv <- c(0, .9)[1] ## change
overlap_res1=NULL
overlap_res2=NULL

######
for (c in 1:length(simnames)){
  set.seed(2024)
  simo <- simulation.sum[[simnames[c]]](n=n, d=d, corrv = corrv)$dta
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
  select2=rownames(m2) 
  
  n_ol=length(intersect(select1,select2))
  
  #overlap_res1[[c]]=c(n1,n2,n_ol)
  overlap_res2[[c]]=c(n1,n2,n_ol)
}

table1=do.call("rbind",overlap_res1) # cor=0
rownames(table1)=simnames

table2=do.call("rbind",overlap_res2) # cor=0.9
rownames(table2)=simnames


###### for larger p real data
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
realDat=realDat2=NULL
for (w in 1:length(data.names)){
  nm <- data.names[w]
  data(list=nm, package = 'datamicroarray')
  x <- eval(parse(text=nm))$x
 if(w == 8){realDat[[w]] <- cbind(x[,-132],x[,132])} else{
#  index=which(colnames(x)==outcomes[w])
  #x <- scale(x)
 # realDat[[w]] <- data.frame(x[,-index],y=x[,index])}
  # x <- scale(x)
  id=which(colnames(x)==outcomes[w])
  realDat[[w]] <- data.frame(x[,-id],y=x[,id])
}}


###### RMSE for real data
rmse <- function(dat,  yid = ncol(dat), lasso = FALSE){
  return <- list()
  dat <- as.data.frame(scale(dat))
  train <- dat[1:round(nrow(dat)*0.8),]
  test <- dat[-c(1:round(nrow(dat)*0.8)),]
  X = train[,-yid]
  Y = train[,yid]
  
  o <- lmv(X = X , Y = Y, lasso = lasso) # , lasso = TRUE
  y.obj <- ymodv(obj = o,gamma = c(seq(0,9, length.out=5)), phi = 0.46)#, rf = FALSE)
  
  pred <- fnpred(mod=y.obj,lmvo = o,newdata = test[,-yid])
  
  return$varguid <- sqrt(colMeans((  matrix(replicate(ncol(pred),test[,yid]),ncol=ncol(pred))-pred)^2, na.rm = TRUE)) 
  
  
  cv_model <- cv.glmnet(x = as.matrix(train[,-yid]) , y = train[,yid], alpha = 1, 
                        nfolds = 10)
  
  #find optimal lambda value that minimizes test MSE
  best_lambda <- cv_model$lambda.min
  o <- glmnet(x = as.matrix(train[,-yid]) , y = train[,yid], alpha = 1, lambda = best_lambda)
  
  pred <- predict(o,newx = as.matrix(test[,-yid]))
  
  return$lasso <- sqrt(mean((test[,yid]-pred)^2)) 
  return
}

table_real=NULL
for (c in 1: length(outcomes)){
  print(c)
  real=realDat[[c]]
  result <- lapply(1:100, function(i) {rmse(dat = real, lasso = TRUE)$varguid}) # 10 repeated train-test
  table_real=rbind(table_real,colMeans(do.call(rbind,result)))
}
### 4
data('pomeroy', package = 'datamicroarray')
dat <- data.frame(pomeroy$x[,-which(colnames(pomeroy$x)=="D28473-s-at")],y=pomeroy$x[,"D28473-s-at"])
result4 <- lapply(1:100, function(i) {rmse(dat = dat, lasso = TRUE)$varguid})
colMeans(do.call(rbind,result4))

### 5
data('shipp', package = 'datamicroarray')
dat <- data.frame(shipp$x[,-which(colnames(shipp$x)=="V2006")],y=shipp$x[,"V2006"])
result5 <- lapply(1:100, function(i) {rmse(dat = dat, lasso = TRUE)$varguid})
colMeans(do.call(rbind,result5))

###8
data('west', package = 'datamicroarray')
dat <- data.frame(west$x[,-132],y=west$x[,132])
result8 <- lapply(1:100, function(i) {rmse(dat = dat, lasso = TRUE)$varguid})
colMeans(do.call(rbind,result8))


data('subramanian', package = 'datamicroarray')
dat <- data.frame(subramanian$x[,-which(colnames(subramanian$x)=="BAX")],y=subramanian$x[,"BAX"])
result10 <- lapply(1:100, function(i) {rmse(dat = dat, lasso = TRUE)$varguid})
colMeans(do.call(rbind,result10))


######10
data('subramanian', package = 'datamicroarray')
dat <- data.frame(subramanian$x[,-which(colnames(subramanian$x)=="BAX")],y=subramanian$x[,"BAX"])
table_real10=NULL
rm(res)
while ( nrow(table_real10) < 100){
  skip_to_next <- FALSE
  tryCatch(res<- rmse(dat = dat, lasso = TRUE)$varguid,
           error = function(e) { message('Caught an error!')
             print(e)
             skip_to_next = TRUE})
  
  if(skip_to_next) { next } 
  table_real10=rbind(table_real10,res)
}
colMeans(table_real10,na.rm = T)

######5
data('shipp', package = 'datamicroarray')
dat <- data.frame(shipp$x[,-which(colnames(shipp$x)=="V2006")],y=shipp$x[,"V2006"])
table_real5=NULL
rm(res)

res<- rmse(dat = dat, lasso = TRUE)$varguid 
table_real5=rbind(table_real5,res)

while ( nrow(table_real5) < 100){
  skip_to_next <- FALSE
  tryCatch(res<- rmse(dat = dat, lasso = TRUE)$varguid,
           error = function(e) { message('Caught an error!')
             print(e)
             skip_to_next = TRUE})
  
  if(skip_to_next) { next } 
  table_real5=rbind(table_real5,res)
}

colMeans(table_real5,na.rm = T)

######4
data('pomeroy', package = 'datamicroarray')
dat <- data.frame(pomeroy$x[,-which(colnames(pomeroy$x)=="D28473-s-at")],y=pomeroy$x[,"D28473-s-at"])
table_real4=NULL
rm(res)

res<- rmse(dat = dat, lasso = TRUE)$varguid # 10 repeated train-test
table_real4=rbind(table_real4,res)

while ( nrow(table_real4) < 100){
  skip_to_next <- FALSE
  tryCatch(res<- rmse(dat = dat, lasso = TRUE)$varguid,
           error = function(e) { message('Caught an error!')
             print(e)
             skip_to_next = TRUE})
  
  if(skip_to_next) { next } 
  table_real4=rbind(table_real4,res)
}

colMeans(table_real4,na.rm = T)



############## OVERLAPPED GENES
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

####### RMSE for real data
# rmse <- c()
# table_real2=NULL
# 
# for (c in 1:length(outcomes)){
#   print(c)
#   real <- realDat[[c]]
#   rmse_real=matrix(data=rep(NA,3*7), nrow=3, ncol=7) # holder k * 6
#   for (k in 1:3){ ### repeat the cv 3 times
#     print(k)
#     folds=createFolds(1:nrow(real), k = 10)
#     rmse=NULL
#   for( i in 1:10){
#     print(i)
#     #trn <- sample.split(1:nrow(real), SplitRatio = 0.75)
#     train  <- as.data.frame(real[-folds[[i]],])
#     test   <- as.data.frame(real[folds[[i]],])
#     data=list(x.train = makeX(train[,1:(ncol(real)-1)]),
#               y.train = train[,ncol(real)],
#               x.test = makeX(test[,1:(ncol(real)-1)]),
#               y.test = test[,ncol(real)])
#     
#     o <- lmv(X =as.matrix(data$x.train) , Y = unlist(data$y.train), lasso = TRUE) # , lasso = TRUE
#     y.obj <-ymodv(o,gamma = c(seq(0,9, length.out=5)), phi = 0.46)#, rf = FALSE)
#     
#     pred <- fnpred(mod=y.obj,lmvo = o,newdata = data$x.test)
#     
#     rmse <- rbind(rmse,sqrt(colMeans((matrix(rep(data$y.test,ncol(pred)),length(data$y.test))-pred)^2,na.rm = TRUE)) )
#   }
#     rmse_real[k,]=colMeans(as.data.frame(rmse),na.rm = TRUE)
#   }
#   table_real2=rbind(table_real2,colMeans(as.data.frame(rmse_real),na.rm = TRUE))
# }
# 
# table_real2$outcome=outcome
# table_real2$dataset=data.names
# tf2=round(table_real2,5)
# 
# saveRDS(tf,"datamicroarray_10RMSE.RDS")
# 
# 
# 
