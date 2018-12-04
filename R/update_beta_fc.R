# Full conditional update of beta
update_beta_fc <- function(z, x, mu, sigma,
                           # Options that can be pre-computed (since they don't depend on z)
                           sigma.inv = chol2inv(chol(sigma)), # The inverse of sigma
                           post.var = chol2inv(chol(t(x) %*% x + sigma.inv)), # The posterior variance of beta
                           post.var.chol = chol(post.var) # Cholesky decomposition of posterior variance
) {
  # The posterior mean
  post.mean <- post.var %*% (t(x) %*% z + sigma.inv %*% mu)
  return( c(rmvnorm(n=1, mu=post.mean, Sigma=post.var, Sigma.chol=post.var.chol)) )
}
