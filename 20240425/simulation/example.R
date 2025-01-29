## Task 1: put a "outcome name" column in the overleaf table 
## Task 2: use the following code the make all the datasets:
##         in total we have 20 datasets for small p case: 10 UCI  and 10 simulated
##                  we have 20 datasets for large p case: 10 from datamicroarray and 10 simulated

###############################################################################
###   
###  small p real data: find 10 datasets from  UCI example
###   
###############################################################################
realdat <- list()
library(mlbench)
data(Ozone)

realdat[[1]] <- Ozone
names(realdat)[1] <- "alon"
####################################################################
###   
###  Simulation data: simulate 10 datasets from the generate_function_simulation.R
### there are 12 simulations there: we don't use "lm" and "lmi"
###   n = 500, d = 15 for low p case
###   n = 100, d = 200 for high p case
####################################################################
## data settings
n <- c(200, 2000)[1]
d <- c(15, 40, 100, 500)[2]
corrv <- c(0, .9)[1]


simnames <- c("cobra2",    "cobra8",    "friedman1", "friedman3", "inx1",      
              "inx2", "inx3",     # "lm",        "lmi",       
              "lmi2",      
              "sup",       "sup2")   

# you can use a number or a name to generate a specific simulation
simo <- simulation.sum[[1]](n=n, d=d, corrv = corrv)
## another way
simo <- simulation.sum[[simnames[1]]](n=n, d=d, corrv = corrv)
## another way
simo <- simulation.sum[["cobra2"]](n=n, d=d, corrv = corrv)
head(simo$dta)

##################################################################################
###   
###  large p real data: find 10 newest datasets from 'datamicroarray' and simulate the outcome
###   
###################################################################################
realDat <- list()
## https://github.com/ramhiser/datamicroarray/tree/master
# library(devtools)
# install_github('ramhiser/datamicroarray')
library(datamicroarray)
data('alon', package = 'datamicroarray')

## use lasso to find the outcome: the continuous outcome will be the most important gene for 
## predicting the classification outcome

library(glmnet)
summary(alon$x[,1:10])
alon$x <- scale(alon$x)
alon$x <- makeX(as.data.frame(alon$x))
cv_model <- cv.glmnet(alon$x,alon$y , family = "binomial")

best_lambda <- cv_model$lambda.min
o <- glmnet(alon$x,alon$y , alpha = 1, lambda = best_lambda, family = "binomial")
yindex <- which.max(abs(as.vector(o$beta)))

realDat[[1]] <- data.frame(y = alon$x[,yindex ],alon$x[,-yindex ])
names(realDat)[1] <- "alon"
