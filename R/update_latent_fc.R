# Full conditional update of latent variables that turn observed ordinal Y
# into continuous z.
update_latent_fc <- function(y, x, beta, sep) {
  # Vector of means for z
  mean <- x %*% beta
  # Vector of lower bounds and upper bounds for z
  extsep <- c(-Inf, sep, Inf)
  lowbound <- rep(0, length(y))
  highbound <- rep(0, length(y))
  for (cat in 1:(length(sep)+1)) {
    lowbound[y == cat] <- extsep[cat]
    highbound[y == cat] <- extsep[cat+1]
  }
  # Generate and return z
  return(truncnorm::rtruncnorm(n=length(y), mean=mean, a=lowbound, b=highbound))
}
