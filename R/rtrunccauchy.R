# Function to sample from a truncated Cauchy distribution
# Truncates the Cauchy(location, scale) distribution to within [a,b]
rtrunccauchy <- function(n, location=0, scale=1, a=-Inf, b=Inf) {
  # "naive" version. Could get tripped up far in tails.
  min.p <- pcauchy(a, location=location, scale=scale)
  max.p <- pcauchy(b, location=location, scale=scale)
  rndunif <- runif(n=n, min=min.p, max=max.p)
  rndcaucny <- qcauchy(p=rndunif, location=location, scale=scale)
}