# Samples from a multivariate normal distribution
# n - number of samples to return
# mu - mean vector for samples
# Sigma - covariance matrix for samples
# Returns a matrix where every row is one sample
rmvnorm <- function(n, mu, Sigma, Sigma.chol=chol(Sigma)) {
  z <- matrix(rnorm(n * length(mu)), nrow=length(mu), ncol=n)
  return( t(t(Sigma.chol) %*% z + c(mu)) )
}
