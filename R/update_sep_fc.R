# Full conditional update of the separators that turn latent variables into
# ordinal response Y
update_sep_fc <- function(y, z, ncat, prior, scale, symmetric) {
  # Only two categories == same separator either case
  if (ncat == 2) return(c(0))
  if (symmetric == TRUE) {
    # Because of symmetry, we only need this many distinct separators
    nsep <- floor((ncat - 1) / 2)
    sep <- rep(0, ncat - 1)
    # For each of those separators, pick a random variable between the
    # min and max possible as determined by z and y. Make sure to check
    # both positive and negative "mirrored" categories.
    # Only make positive separators, then mirror to make the negative ones.
    for (i in (ncat - nsep):(ncat - 1)) {
      lbound <- max(0, abs(z[(y >= ncat + 1 - i) & (y <= i)]))
      rbound <- min(Inf, abs(z[(y < ncat + 1 - i) | (y > i)]))
      sep[i] <- rtruncx(dist=prior, n=1, scale=scale, a=lbound, b=rbound)
    }
    # Sort, to fix cases of empty categories, and mirror for symmetry
    sep <- sort(sep) - sort(sep, decreasing=TRUE)
  } else {
    # Now we need all separators individually. But in the non-symmetric
    # case the first separator is always fixed at zero
    sep <- rep(0,ncat - 1)
    for (i in (2:(ncat - 1))) {
      lbound <- max(0, z[y <= i])
      rbound <- min(Inf, z[y > i])
      sep[i] <- rtruncx(dist=prior, n=1, scale=scale, a=lbound, b=rbound)
    }
    # Sort, to fix cases of empty categories
    sep <- sort(sep)
  }
  return(sep)
}

# Function to generically sample from one of the many allowed
# truncated full conditional distributions for the separators
rtruncx <- function(dist, n, scale, a, b) {
  if (dist == "exp") {
    return(rtruncexp(n=n, rate=1/scale, a=a, b=b))
  } else if (dist == "norm") {
    return(truncnorm::rtruncnorm(n=n, sd=scale, a=a, b=b))
  } else if (dist == "cauchy") {
    return(rtrunccauchy(n=n, scale=scale, a=a, b=b))
  } else if (dist == "unif") {
    return(runif(n=n, min=a, max=b))
  } else {
    stop("Unallowed prior for separators.")
  }
}
