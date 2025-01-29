# library(plotly) # 3D plot
# library(ggplot2)
#install.packages("/Volumes/Extreme SSD/heteroscedasticity/R code/cvxclustr_1.1.0.tar.gz", repos = NULL, type = "source")
library(cvxclustr) # convex clustering  cvxclustr::kernel_weights

library(randomForestSRC)
#####################
# convex clustering
#####################

covx <- function(x, knn = 9, phi = 0.46,  
                 gamma = c(seq(0,8.56, length.out=5)), w = NULL){ # knn if wants knn kernel
  x <- as.data.frame(x)
  nums <- unlist(lapply(x, is.numeric))  
  X <- t(scale(as.matrix(x[,nums])))
  n <- ncol(X)
  ## Pick some weights and a sequence of regularization parameters.
  if (is.null(w)){
  w <- kernel_weights(X,phi)
  if (!is.null(knn)){
    w <- knn_weights(w,knn,n) } 
  } 
  ## Perform clustering
  list(sol = cvxclust(X,w,gamma, nu = 1/n), w = w, x = x[,nums], x.all = x, gamma = gamma, knn = knn )
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
        
        yhat.rlm <- predict(mod.rlm,newdata=dat.test)
        sqrt(mean((y.new-yhat.rlm)^2)) 
      }}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
)
list(rmse = unlist(mlm),q = unlist(q), gamma = obj$gamma)
}
#####################################################
# decide final model after optimizing gamma and phi
######################################################
fnmod <- function(dat.x,dat.y,knn = 10, gamma = c(seq(0,8.56, length.out=5)), phi = 0.45, rf = TRUE)
{  w <- rfobj <- NULL
if (rf == TRUE){
  rfo0 <- rfgnr(formula = Y~.,data= data.frame(Y = as.vector(dat.y),
                                     dat.x), proximity = TRUE)
  rfobj <- rfo0$obj
  w <- rfo0$w
  
} 
data <- as.data.frame(dat.x)
#nums <- unlist(lapply(data, is.numeric))  
obj <- covx(x=as.matrix(dat.x),knn = knn, # as.matrix(dat.x[,nums])
            gamma = gamma, phi = phi, w=w)
w <- obj$w
n <- length(obj$sol$U[[1]][1,])
A <- create_adjacency(obj$sol$V[[1]],w,n)
rdef.all <- find_clusters(A)
data$z <- as.factor(as.character(rdef.all$cluster))
data$y <- dat.y
center <- cent(dat = dat.x,cluster = rdef.all$cluster)
list( clust = obj,
      data = data,
      center = center,
      rfo = rfobj)
}
#####################################################
# prediction using final model 
######################################################
rfgnr <- function(formula, data, proximity = TRUE){
 
  rfo <- rfsrc(formula,data= data,proximity = proximity ) 
  b <- rfo$proximity

  list(obj = rfo,
       w = ifelse(proximity, c(t(b)[lower.tri(t(b), diag=FALSE)]), NA) )
}

fnpred <- function(mod,lmvo,newdata){
  dat.test <- as.data.frame(newdata)
 # nums <- unlist(lapply(dat.test, is.numeric))  
  dat.test$z <- as.character(clusters(newdata[,which(lmvo$obj.varGuid$beta != 0)],mod$center))
  
  mod$clust
 if (length(which(class(lmvo$obj.varGuid) == "glmnet"))==0) {
   
  
  yhat0 <- yhat <- yhat2 <- predict(lmvo$obj.varGuid,newdata=dat.test)
  yhat0b <- yhatb <- yhat2b <- predict(lmvo$obj.OLS,newdata=dat.test)
 } else {
   yhat0 <- yhat <- yhat2 <- c(predict(lmvo$obj.varGuid,
                                          newx=as.matrix(dat.test[,-ncol(dat.test)])))
   yhat0b <- yhatb <- yhat2b <- c(predict(lmvo$obj.lasso,
                                          newx=as.matrix(dat.test[,-ncol(dat.test)])))
 }
 # if (!((length(unique(mod$data$z))==1)|(length(unique(mod$data$z))==nrow(mod$data)))) {
    
    if (is.null(mod$rfo)){
    rfo0 <- rfgnr(formula = Y~.,
                  data= data.frame(Y = c(lmvo$obj.varGuid$residuals), #lmvo$obj.varGuid$model[,1] ,
                      subset(lmvo$obj.varGuid$model, select = -c(1,ncol(lmvo$obj.varGuid$model)))),
                  proximity = FALSE)
    rfo <- rfo0$obj } else {
      rfo <- mod$rfo
    }
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

  yhat <- data.frame(
       yhatVarGuidOriginal = yhat0,
       yhatVarGuid1 = yhat,
       yhatVarGuid2 = yhat2,
       yhatVarGuid3 = yhatb,
       yhatVarGuid4 = yhat2b,
       yhatOLS = yhat0b)
  if (length(which(class(lmvo$obj.varGuid) == "glmnet"))>0) {
    colnames(yhat)[ncol(yhat)] <- "yhatLasso"
  }
  yhat
}
#sm2 <- summary(mod.rlm)
#print(sm2, digits=2, corr=T)   
ymodv <- function(obj, knn = 10, gamma = c(seq(0,8.56, length.out=5)), phi = 0.45, rf = TRUE){
  dat <- obj$obj.varGuid$model
  colnames(dat)[ncol(dat)] <- "weights"
  
  mod <- fnmod( dat.x=subset(dat, select = -c(Y,weights)),
               dat.y= obj$obj.varGuid$residuals,
               knn = knn,
               gamma = gamma, phi = phi,
               rf = rf)
  mod
}

