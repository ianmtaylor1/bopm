# Truncated exponential sampler
# n, rate - usual parameters for rexp
# a, b - low and high truncation points, respectively
rtruncexp <- function(n, a=0, b=Inf, rate=1) {
  a[a < 0] <- 0
  stopifnot(a <= b)
  unif.max <- pexp(q=b-a, rate=rate)
  rnd.unif <- runif(n=n, min=0, max=unif.max)
  rnd.exp <- a + qexp(p=rnd.unif, rate=rate)
  return( pmin(pmax(rnd.exp, a), b) )
}
