library(bopm)
library(coda)
library(parallel)

rm(list=ls())

n <- 1000

data <- data.frame(A=rnorm(n), B=rnorm(n), C=rnorm(n))

beta <- c(-2,1,4)
intercept <- 3

cutoffs <- c(0,2,4,6)

data["z"] <- intercept + c(data.matrix(data) %*% beta) + rnorm(n)
data["y"] <- rep(1,n)
for (i in 1:length(cutoffs)) {
  data[(data["z"] > cutoffs[i]),"y"] <- i + 1
}

cl <- makeCluster(3)

results <- bopm.parallel(y~A+B+C, data, cluster=cl,
                         n.chains=3, n.iter=10000,
                         beta.covar=100*diag(4), threshold.scale=2,
                         print=TRUE)

stopCluster(cl)

summary(results)

traceplot(results)
