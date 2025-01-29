####################################
# real dataset example
####################################
source("./20240412/leash2.0.R")
source("./20240412/varGuid20240412.R")
load("./20240412/realdataexample.RData")

#######################
## BostonHousing2 ##
knn = 10
phi = 0.46
gamma = 9#c(8,9)#c(seq(8,12.56, length.out=4))
          
rmse <- c()

x <- as.data.frame(BostonHousing2.train.x)
nums <- unlist(lapply(x, is.numeric)) 

for (i in 1:4){
print(i) 
trn <- sample(1:506,400)

data=list(x.train = rbind(BostonHousing2.train.x,BostonHousing2.test.x)[trn,nums],
          y.train = c(BostonHousing2.train.y,BostonHousing2.test.y)[trn],
          x.test = rbind(BostonHousing2.train.x,BostonHousing2.test.x)[-trn,nums],
          y.test = c(BostonHousing2.train.y,BostonHousing2.test.y)[-trn])

o <- lmv(X = data$x.train, Y = data$y.train)
y.obj <- ymodv(o,knn = knn, gamma = gamma, phi = phi)


pred <- fnpred(mod=y.obj,lmvo = o,newdata = data$x.test)

rmse <- rbind(rmse,sqrt(colMeans((matrix(rep(data$y.test,ncol(pred)),length(data$y.test))-pred)^2)) )

}
rmse
colMeans(as.data.frame(rmse))
rmse <- as.list(as.data.frame(rmse))
boxplot(rmse,ylab = "Prediction Error",main="BonstonHousing2")

