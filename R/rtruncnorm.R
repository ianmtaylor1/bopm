# Truncated univariate normal sampler
# n, mean, sd - usual parameters for rnorm
# a, b - low and high truncation points, respectively
rtruncnorm <- function(n, a=-Inf, b=Inf, mean=0, sd=1) {
  stopifnot(a <= b)
  # "Flip" things to the right tail if most of the truncated area tails to the right
  lhs <- abs(a - mean) > abs(b - mean)
  # Find log quantiles of the endpoints
  log.qa <- pnorm(a, mean=mean, sd=sd, log.p=TRUE, lower.tail=lhs)
  log.qb <- pnorm(b, mean=mean, sd=sd, log.p=TRUE, lower.tail=lhs)
  # Generate truncated exponential r.v. between -log.qb and -log.qa
  lb.exp <- pmin(-log.qb, -log.qa)
  ub.exp <- pmax(-log.qb, -log.qa)
  rnd.exp <- rtruncexp(n=n, a=lb.exp, b=ub.exp)
  # Convert to normal r.v.
  rnd.norm <- qnorm(p=-rnd.exp, mean=mean, sd=sd, log.p=TRUE, lower.tail=lhs)
  return( pmin(pmax(rnd.norm, a), b) )
}
