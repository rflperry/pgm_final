## Imports
library(vars)
library(lars)
library(simone)
library(igraph)
library(bnlearn)
library(penalized)
library(forecast)
library(ggplot2)
library(tidyr)
library(dplyr)



## Setup
set.seed(100)
setwd("~/JHU/Spring2019/PGM/final_project/finalproject")
par(mar=c(1,1,1,1))
## Load Data
y <- read.csv("./data/six_var_1980_percent_changes.csv",header=TRUE)
fedfund <- "Federal_Funds"
test_size <- 24
## Play With Data
summary(VAR(y[,-c(5,6,7)],p=4))
y.var <- VAR(y[,-c(5,6,7)],lag.max = 4, ic = "SC")

stab <- stability(y.var, type = "OLS-CUSUM")
plot(stab)

normality.test(y.var)

## AR elements
order <- 3
y_temp <- y[-c(1:order),]
for(i in 1:order){
  #ar[paste(fedfund,"_t",i,sep="")] = y[(order-i+1):(N-i+1),fedfund]
  y_temp[,paste(fedfund,"_t",i,sep="")] <- y[(order-i+1):(nrow(y)-i),fedfund]
}
y <- y_temp

dates <- y[,1]
y <- ts(y)
y <- y[,-1]
vars <- colnames(y)
N <- length(y[,1])

## Plot Data
autoplot(y[,-c(6,7,8)],facets=FALSE,xlab = "Quarters Since January 1980", 
         ylab = "Percent/Percent Change")#, title = "Quarterly Macroeconomic Trends")

y.train <- y[1:(N-test_size),]
y.test <- y[(N-test_size):N,]

## LASSO learning on inflation (idx 1)
predict_idx <- 1 
response.train <- y.train[-1,predict_idx]
predictors.train <- y.train[-nrow(y.train),]
  
lasso.fit <- lars(x=predictors.train, y=response.train, type="lasso")
#lasso.fit <- lapply(colnames(y.train),
#                  function(n){
#                    lars(y = y.train[-1,n], x = y.train[-nrow(y.train),], type = "lasso")
#                  })

## We want examine the model parameters for the to be predicted one
plot(lasso.fit)
coef(lasso.fit)

## Cross-validation with cv.lars to select the edges
lasso.cv <- cv.lars(x = predictors.train, y = response.train, 
                    type="lasso")
frac <- lasso.cv$index[which.min(lasso.cv$cv)]
clip(0,1, 0, 4.5)
abline(v=frac,col=c("red"), lty=c(2), lwd=c(3))

#lasso.cv <- lapply(colnames(y.train),
#                    function(n){
#                      cv.lars(y = y.train[-1,n], x = y.train[-nrow(y.train),])
#                    })

## Create significant coefficient matrix
#adj <- c()
#for (i in c(1:8)){
#  frac <- lasso.cv[[i]]$index[which.min(lasso.cv[[i]]$cv)]
#  #frac <- lasso.cv$index[which.min(lasso.cv$cv)]
#  coeffs <- stats::predict(lasso.fit[[i]], s=frac, type="coef", 
#                           mode="fraction")$coefficients
#  adj <- c(adj,coeffs)
#}
#adj <- matrix(adj, ncol=8,dimnames=list(vars,vars))
#adj <- 1*(abs(adj)>0.05)


## Find the minimized MSE fraction coefficients

lasso.s <- stats::predict(lasso.fit, s=frac, type="coef", 
                         mode="fraction")

## G1DBN
library(G1DBN)
step1 <- DBNScoreStep1(y.train, method="ls")
edgesG1 <- BuildEdges(score=step1$S1ls, threshold=0.5, prec=6)
nrow(edgesG1)

step2 = DBNScoreStep2(step1$S1ls, data = y.train, 
                      method = "ls", alpha1 = 0.50)

edgesG2 <- BuildEdges(score = step2,
                      threshold = 0.05, prec = 6)

nrow(edgesG2)

## Inference
## bn.fit.qqplot(), bn.fit.xyplot() and bn.fit.histogram()

### Estimated Models
## Train Lasso model
causes <- names(coef(lasso.s))[lasso.s$coefficients != 0]
dbn.lasso <- empty.graph(c("CPI0",causes))
for (node in causes)
  dbn.lasso = set.arc(dbn.lasso, node, "CPI0")
dbn.lasso.data = as.data.frame(predictors.train[,nodes(dbn.lasso)[-c(1)]])
dbn.lasso.data[, "CPI0"] = response.train
dbn.lasso.fit = bn.fit(dbn.lasso, dbn.lasso.data)
dbn.lasso.bic <- BIC(dbn.lasso.fit, dbn.lasso.data)

## Test Lasso Model
dbn.lasso.data.test = as.data.frame(predictors.test[,nodes(dbn.lasso)[-c(1)]])
dbn.lasso.data.test[, "CPI0"] = response.test
dbn.lasso.predict <- predict(dbn.lasso, node="CPI0", dbn.lasso.data.test,method = "bayes-lw")

dbn.lasso.acc <- forecast::accuracy(f = dbn.lasso.predict, x = response.test)

## Train G1 model
dbn.G1 = empty.graph(c("CPI0","CPI","Federal_Funds"))
dbn.G1 = set.arc(dbn.G1, "CPI", "CPI0")
dbn.G1 = set.arc(dbn.G1, "Federal_Funds", "CPI0")
dbn.G1.data = as.data.frame(predictors.train[,nodes(dbn.G1)[-c(1)]])
dbn.G1.data[, "CPI0"] = response.train
dbn.G1.fit = bn.fit(dbn.G1, dbn.G1.data)
dbn.G1.bic <- BIC(dbn.G1.fit, dbn.G1.data)

## Test G1 model
dbn.G1.data.test = as.data.frame(predictors.test[,nodes(dbn.G1)[-c(1)]])
dbn.G1.data.test[, "CPI0"] = response.test
dbn.G1.predict <- predict(dbn.G1, node="CPI0", dbn.G1.data.test,method = "bayes-lw")

dbn.G1.acc <- forecast::accuracy(f = dbn.G1.predict, x = response.test)

#cpquery(dbn.lasso.fit, event = (`CPI0` < 1))

### Real Model
## Train
dbn.real = empty.graph(c("CPI0","CPI","Federal_Funds",
                     "Wages","GDP","Labor_output"))
dbn.real = set.arc(dbn.real, "CPI", "CPI0")
dbn.real = set.arc(dbn.real, "Wages", "CPI0")
dbn.real = set.arc(dbn.real, "Federal_Funds", "CPI0")
dbn.real = set.arc(dbn.real, "Labor_output", "CPI0")
dbn.real = set.arc(dbn.real, "GDP", "CPI0")

dbn.real.data.test = as.data.frame(predictors.train[,nodes(dbn.real)[-c(1)]])
dbn.real.data.test[, "CPI0"] = response.train
dbn.real.fit = bn.fit(dbn.real, dbn.real.data.test)
dbn.real.predict <- predict(dbn.real, node="CPI0", dbn.real.data.test,method = "bayes-lw")

dbn.real.bic <- BIC(dbn.real.fit, data=dbn.real.data.test)

## Test
dbn.real.data.test = as.data.frame(predictors.test[,nodes(dbn.real)[-c(1)]])
dbn.real.data.test[, "CPI0"] = response.test
dbn.real.predict <- predict(dbn.real, node="CPI0", dbn.real.data.test,method = "bayes-lw")

dbn.real.acc <- forecast::accuracy(f = dbn.real.predict, x = response.test)

## Plot test results
range.full <- c((N-test_size-7):N)
range.test <- c((N-test_size+1):N)
#True Values
plot(range.full,y[,predict_idx][range.full],pch=20,col='black',
     main='Interest Rate Predictions',xlab='Quarter After January 1980',
     ylab='Interest Rate (%)',
     ylim=c(min(y[,predict_idx][range.full]),
            max(dbn.lasso.predict, dbn.G1.predict, dbn.real.predict,y[,predict_idx][range.full])))
lines(range.full,y[,predict_idx][range.full],col='black')
#axis(1,at= range.full,labels=levels(dates)[range.full],las=2)

# Lasso Model Predictions
points(range.test,dbn.lasso.predict,pch=21,col='red')
lines(range.test,dbn.lasso.predict,pch=21,col='red')
# G1 Model Predictions
points(range.test,dbn.G1.predict,pch=21,col='springgreen3')
lines(range.test,dbn.G1.predict,pch=21,col='springgreen3')
# Real Model Predictions
points(range.test,dbn.real.predict,pch=21,col='blue')
lines(range.test,dbn.real.predict,pch=21,col='blue')
#Legend
legend("bottomright", legend=c("True Inflation", "LARS", "G1DBN", "Expert"),
       col=c('black','red','springgreen3', 'blue'),pch=c(20,20,20,20),lty=c(1,2,2,2))
