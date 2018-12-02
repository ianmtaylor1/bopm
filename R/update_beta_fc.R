# Full conditional update of beta
update_beta_fc <- function(z, x, mu, sigma) {
  sigma.inv <- chol2inv(chol(sigma))
  # The posterior variance
  post.var <- chol2inv(chol(t(x) %*% x + sigma.inv))
  # The posterior mean
  post.mean <- post.var %*% (t(x) %*% z + sigma.inv %*% mu)
  return( c(rmvnorm(n=1, mu=post.mean, Sigma=post.var)) )
}
