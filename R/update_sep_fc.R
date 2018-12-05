# Full conditional update of the separators that turn latent variables into
# ordinal response Y
update_sep_fc <- function(y, z, ncat, sd) {
  if (ncat == 2) return(c(0))
  # Because of symmetry, we only need this many distinct separators
  nsep <- (ncat - 1) / 2
  sep <- rep(0, ncat - 1)
  # For each of those separators, pick a random variable between the
  # min and max possible as determined by z and y. Make sure to check
  # both positive and negative "mirrored" categories
  for (i in 1:nsep) {
    lbound <- max(-Inf, z[y == i], -z[y == ncat + 1 - i])
    rbound <- min(0, z[y == i + 1], -z[y == ncat - i])
    sep[i] <- rtruncnorm(n=1, sd=sd, a=lbound, b=rbound)
  }
  # Sort, to fix cases of empty categories, and mirror
  sep <- sort(sep) - sort(sep, decreasing=TRUE)
  return(sep)
}
