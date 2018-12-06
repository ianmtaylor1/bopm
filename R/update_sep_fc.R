# Full conditional update of the separators that turn latent variables into
# ordinal response Y
update_sep_fc <- function(y, z, ncat, scale, symmetric) {
  if (ncat == 2) return(c(0))
  if (symmetric == TRUE) {
    # Because of symmetry, we only need this many distinct separators
    nsep <- floor((ncat - 1) / 2)
    sep <- rep(0, ncat - 1)
    # For each of those separators, pick a random variable between the
    # min and max possible as determined by z and y. Make sure to check
    # both positive and negative "mirrored" categories
    for (i in (ncat - nsep):(ncat - 1)) {
      lbound <- max(0, abs(z[(y >= ncat + 1 - i) & (y <= i)]))
      rbound <- min(Inf, abs(z[(y < ncat + 1 - i) | (y > i)]))
      sep[i] <- rtruncexp(n=1, rate=1/scale, a=lbound, b=rbound)
    }
    # Sort, to fix cases of empty categories, and mirror for symmetry
    sep <- sort(sep) - sort(sep, decreasing=TRUE)
  } else {

    stop("Non-symmetric case not yet implemented.")

  }
  return(sep)
}
