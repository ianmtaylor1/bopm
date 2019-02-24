library(bopm)
library(coda)
library(parallel)

rm(list=ls())

n <- 1000
testn <- 5
testreps <- 1000
testdata <- data.frame(A=rnorm(testn), B=rnorm(testn), C=rnorm(testn))
alldata <- rbind(
  data.frame(A=rnorm(n), B=rnorm(n), C=rnorm(n)),
  testdata[rep(1:testn, each=testreps), ]
)

beta <- c(-2,1,4)
intercept <- 3
cutoffs <- c(0,2,4,6)

alldata["z"] <- intercept + c(data.matrix(alldata) %*% beta) + rnorm(n)
alldata["y"] <- rep(1,dim(alldata)[1])
for (i in 1:length(cutoffs)) {
  alldata[(alldata["z"] > cutoffs[i]),"y"] <- i + 1
}

builddata <- alldata[1:n,]
testdata <- alldata[(n+1):dim(alldata)[1],]

cl <- makeCluster(3)
results <- bopm.parallel(y~A+B+C, builddata, cluster=cl,
                         n.chains=3, n.iter=15000,
                         beta.covar=100*diag(4),
                         threshold.scale=2, threshold.prior="unif")
stopCluster(cl)

summary(results)

traceplot(results)

# # Test if the probabilities are accurate
# testresults <- predict(results, testdata)
# for (i in 1:testn) {
#   cat(round(testresults[(i-1)*testreps+1,], digits=2), "\n")
#   print(round(table(testdata[((i-1)*testreps+1):(i*testreps),"y"])/testreps, digits=2))
#   cat("\n\n\n")
# }


