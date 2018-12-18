# Create a summary of a bopm object
#' @export
summary.bopm <- function(object, ...) {
  # Get the summary of the underlying MCMC.list object
  s <- NextMethod()
  # Add to it
  s$formula <- attr(object, "formula")
  s$n <- dim(attr(object, "build.data"))[1]
  s$ystats <- rep(0, attr(object, "ncat"))
  mf <- model.frame(attr(object, "formula"), attr(object, "build.data"), na.action=na.fail)
  y <- c(mf[,1])
  for (i in 1:attr(object, "ncat")) {
    s$ystats[i] <- sum(y == i)
  }
  s$symmetric <- attr(object, "symmetric")
  class(s) <- c("summary.bopm", class(s))
  return(s)
}
