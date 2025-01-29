# library(plotly) # 3D plot
# library(ggplot2)
#install.packages("/Volumes/Extreme SSD/heteroscedasticity/R code/cvxclustr_1.1.0.tar.gz", repos = NULL, type = "source")
library(cvxclustr) # convex clustering

library(randomForestSRC)
#####################
# convex clustering
#####################

covx <- function(x, knn = 9, phi = 0.46,  
                 gamma = c(seq(0,8.56, length.out=5))){ # knn if wants knn kernel
  x <- as.data.frame(x)
  nums <- unlist(lapply(x, is.numeric))  
  X <- t(scale(as.matrix(x[,nums])))
  n <- ncol(X)
  ## Pick some weights and a sequence of regularization parameters.
  w <- kernel_weights(X,phi)
  if (!is.null(knn)){
    k <- knn
    w <- knn_weights(w,k,n) } 
  ## Perform clustering
  list(sol = cvxclust(X,w,gamma), w = w, x = x[,nums], x.all = x, gamma = gamma, knn = knn )
}
#####################
# plot path
#####################
plotpath <- function (sol){ 
  nGamma <- sol$nGamma
  df.paths <- data.frame(x=c(),y=c(), group=c())
  for (j in 1:nGamma) {
    pcs <- sol$U[[j]]
    x <- pcs[1,]
    y <- pcs[2,]
    df <- data.frame(x.1=pcs[1,], x.2=pcs[2,], group=1:length(x))
    df.paths <- rbind(df.paths,df)
  }
  data_plot <- ggplot(data=df.paths,aes(x=x.1,y=x.2))
  data_plot <- data_plot + geom_path(aes(group=group),colour='grey30',alpha=0.5)
  data_plot <- data_plot + xlab('pcs.1') + ylab('pcs.2')
  data_plot + theme_bw()
}
#####################################################
# fake random effect from cluster object
######################################################
fakez <- function(obj){      ### extract clusters from a convex object
  sol <- obj$sol
  w <- obj$w
  n <- length(sol$U[[1]][1,])
  lapply(1:sol$nGamma,function(i){
    A <- create_adjacency(sol$V[[i]],w,n)
    find_clusters(A)
  })
}

cent <- function(dat,cluster)  ### calculate centers from clusters of training data
{ x <- data.frame(matrix(NA,length(unique(cluster)),length(dat[1,])))
colnames(x) <- colnames(dat) 
for (i in sort(unique(cluster)))
{ if (length(which(cluster==i))==1){x[i,] <- dat[which(cluster==i),]}
  else {x[i,] <- colMeans(dat[which(cluster==i),])}} 
x
}
clusters <- function(x, centers) { ### calculate clusters for test data according to cluster centers of training data
  # compute squared euclidean distance from each sample to each cluster center
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  max.col(-t(tmp))  # find index of min distance
}
#####################################################
# optimize random effect # in mixed linear model
######################################################
chooseq <- function(obj,y.obj,x.new,y.new)
{ rdef.all<-fakez(obj)
dat.train <- lapply(1:length(rdef.all),function(i){
  dat.train <- as.data.frame(scale(as.matrix(obj$x)))
  dat.train$y <- y.obj
  dat.train$z <- as.factor(as.character(rdef.all[[i]]$cluster))
  dat.train
})
q=lapply(1:length(dat.train),function(i){length(levels(dat.train[[i]]$z))})
dat.test=as.data.frame(x.new)
nums <- unlist(lapply(dat.test, is.numeric))
dat.test[,nums] <- scale(x.new[,nums])
mlm <- lapply(1:length(dat.train),function(i){
  tryCatch({
    if (length(rdef.all[[i]]$size)>(length(y.obj)-ncol(x.new)-1)) {
      return(NA)} else {if (length(rdef.all[[i]]$size)==1){
        mod.rlm <- lm(as.formula(paste("y~",paste(colnames(dat.test[,nums]),collapse = "+"))), 
                      dat.train[[i]])
      } else{
        mod.rlm <- lmer(as.formula(paste("y~",paste(colnames(dat.test[,nums]),collapse = "+"),"+(1 | z)+(0)")), 
                        dat.train[[i]])}
        center <- cent(dat = obj$x,cluster = rdef.all[[i]]$cluster)
        
        dat.test$z <- as.factor(as.character(clusters(x.new[,nums],center)))
        
        yhat.rlm <- predict(mod.rlm,as.matrix(dat.test))
        sqrt(mean((y.new-yhat.rlm)^2)) 
      }}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
)
list(rmse = unlist(mlm),q = unlist(q), gamma = obj$gamma)
}
#####################################################
# decide final model after optimizing gamma and phi
######################################################
fnmod <- function(dat.x,dat.y,knn = 10, gamma = c(seq(0,8.56, length.out=5)), phi = 0.45)
{  data <- as.data.frame(dat.x)
nums <- unlist(lapply(data, is.numeric))  
obj <- covx(as.matrix(dat.x[,nums]),knn = knn,
            gamma = gamma,phi = phi)
w <- obj$w
n <- dim(obj$sol$U[[1]])[2]
A <- create_adjacency(obj$sol$V[[1]],w,n)
rdef.all <- find_clusters(A)
data$z <- as.factor(as.character(rdef.all$cluster))
data$y <- dat.y
#if ((length(unique(data$z))==1)|(length(unique(data$z))==nrow(data))){linear <- lm(as.formula(paste("y~",paste(colnames(as.data.frame(dat.x)),collapse = "+"))),data)} else{
#  linear <- lmer(as.formula(paste("y~",paste(colnames(as.data.frame(dat.x)),collapse = "+"),"+(1 | z)+(0)")),data)}
if (dim(dat.x)[2] == 1) dat.x <- cbind(dat.x,dat.x)
center <- cent(dat = dat.x,cluster = rdef.all$cluster)
list( clust = obj,
    #  linear = linear, 
      data = data,
    #  x = dat.x,
      center = center)
}
#####################################################
# prediction using final model 
######################################################
fnpred <- function(mod,lmvo,newdata){
  newdata <- as.matrix(newdata)
  dat.test <- as.data.frame(newdata)
  nums <- unlist(lapply(dat.test, is.numeric))  
  if (dim(newdata)[2] == 1) {
    newdata <- data.frame(X = as.vector(newdata))
    nums <- c(1,1)
  }
  if (length(mod$center) == 0) { dat.test$z <- NA
  mod$data$z <- lmvo$obj.varGuid$model$Y
  } else {
  dat.test$z <- as.character(clusters(newdata[,nums],mod$center))
  }

  if (length(lmvo$obj.OLS)==0){
    lmvo$obj.OLS <- lmvo$obj.lasso
    yhat0 <- yhat <- yhat2 <- predict(lmvo$obj.varGuid,as.matrix(newdata))
    yhat0b <- yhatb <- yhat2b <- predict( lmvo$obj.OLS,as.matrix(newdata))
  } else {
    yhat0 <- yhat <- yhat2 <- predict(lmvo$obj.varGuid,as.data.frame(newdata))
    yhat0b <- yhatb <- yhat2b <- predict( lmvo$obj.OLS,as.data.frame(newdata))
  }
  
 # if (!((length(unique(mod$data$z))==1)|(length(unique(mod$data$z))==nrow(mod$data)))) {
    datrf <- data.frame(Y = c(lmvo$obj.varGuid$residuals), #lmvo$obj.varGuid$model[,1] ,
                              subset(lmvo$obj.varGuid$model, select = -c(1,ncol(lmvo$obj.varGuid$model))) )
    if (dim(datrf)[2] == 1){
      datrf <- data.frame(Y = c(lmvo$obj.varGuid$residuals), #lmvo$obj.varGuid$model[,1] ,
                          lmvo$X )
    }
    rfo <- rfsrc(Y~., data= datrf)
    
   # rfo$predicted.oob - lmvo$obj.varGuid$model[,1]
    
    mod$data$y <- rfo$predicted.oob
    ycenter <- aggregate(y~z,data = mod$data,mean) 
    
    
    yhat <- yhat0 + ycenter$y[match(dat.test$z,ycenter$z)]   
    yhatb <- yhat0b + ycenter$y[match(dat.test$z,ycenter$z)]   
    
    resd <- abs(lmvo$obj.varGuid$residuals)-sqrt(abs(lmvo$res$fitted.values))
   # mod$data$y <- abs(resd)*sign(lmvo$obj.varGuid$residuals)
    mod$data$y[which(resd<0)] <- 0
    ycenter <- aggregate(y~z,data = mod$data,mean) 
    yhat2 <- yhat0 + ycenter$y[match(dat.test$z,ycenter$z)]   
    yhat2b <- yhat0b + ycenter$y[match(dat.test$z,ycenter$z)]   
 # }

  data.frame(
       yhatVarGuidOriginal = yhat0,
       yhatVarGuid1 = yhat,
       yhatVarGuid2 = yhat2,
       yhatVarGuid3 = yhatb,
       yhatVarGuid4 = yhat2b,
       yhatOLS = yhat0b)
}
#sm2 <- summary(mod.rlm)
#print(sm2, digits=2, corr=T)   
ymodv <- function(obj, knn = 10, gamma = c(seq(0,8.56, length.out=5)), phi = 0.45){
  dat <- obj$obj.varGuid$model
  colnames(dat)[ncol(dat)] <- "weights"
  
  mod <- fnmod( dat.x=subset(dat, select = -c(Y,weights)),
              # dat.x=data.frame(datY=dat$Y,resY=o$obj.varGuid$residuals,varhY=o$res$fitted.values),
               dat.y=dat$Y,
               knn = knn,
               gamma = gamma, phi = phi)
  mod
}

