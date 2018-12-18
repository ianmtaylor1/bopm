library(bopm)
library(coda)

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


results <- bopm(y~A+B+C, data, n.chains=6, n.iter=5000, beta.covar=100*diag(4))

traceplot(results)
