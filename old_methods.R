dbn3 = empty.graph(c("CPI0","CPI","Federal_Funds"))
dbn3 = set.arc(dbn3, "CPI", "CPI0")
dbn3 = set.arc(dbn3, "Federal_Funds", "CPI0")

dbn3.data = as.data.frame(predictors.train[,nodes(dbn3)[-c(1)]])
dbn3.data[, "CPI0"] = response.train
dbn3.fit = bn.fit(dbn3, dbn3.data)
dbn3.predict <- predict(dbn3, node="CPI0", dbn3.data.test,method = "bayes-lw")

BIC(dbn3.fit, data=dbn3.data)

## Test
dbn3.data.test = as.data.frame(predictors.test[,nodes(dbn3)[-c(1)]])
dbn3.data.test[, "CPI0"] = response.test
dbn3.predict <- predict(dbn3, node="CPI0", dbn3.data.test,method = "bayes-lw")

forecast::accuracy(f = dbn3.predict, x = response.test)

###################3 Simone
## DBN BIC for VAR(1)
simone.fit <- simone(y.train, type="time-course", cluster=FALSE, 
                     control = setOptions(clusters.crit = "BIC"))
best_fit <- which.max(simone.fit$BIC)

## See results
plot(simone.fit, output="BIC")
#abline(v=which.max(simone.fit$BIC), col=c("red"), lty=c(2))

simone.fit$n.edges[best_fit]

cov <- simone.fit$networks[best_fit][[1]]

simone.g <- as.directed(graph.adjacency(cov!=0))
plot(simone.g,label.cex=0.05,vertex.label.color=c('blue'),edge.color=c('black'),
     vertex.color=c('white'), vertex.size=19, vertex.label = vars)

#########################
predictors.test <- y.test[1:(nrow(y.test)-1),]
response.test <- y.test[-1,predict_idx]

lasso.est <- predict(lasso.fit, type="fit",newx=response.test, s=frac, mode="fraction")

###############################
### Estimated Model
lambda <- optL1(response = response.train, penalized = predictors.train)$lambda
lasso.s <- penalized(response = response.train, penalized = predictors.train, lambda1=lambda)

## Train
dbn2 <- empty.graph(c(names(coef(lasso.s))[-c(1)],"CPI0"))
for (node in names(coef(lasso.s))[-c(1)])
  dbn2 = set.arc(dbn2, node, "CPI0")
dbn2.data = as.data.frame(predictors.train[,nodes(dbn2)[-c(5)]])
dbn2.data[, "CPI0"] = response.train
dbn2.fit = bn.fit(dbn2, dbn2.data)
#dbn2.fit[["CPI0"]] = lasso.s

## Test
dbn2.data.test = as.data.frame(predictors.test[,nodes(dbn2)[-c(5)]])
dbn2.data.test[, "CPI0"] = response.test
dbn2.predict <- predict(dbn2, node="CPI0", dbn2.data.test,method = "bayes-lw")

forecast::accuracy(f = dbn2.predict, x = response.test)

