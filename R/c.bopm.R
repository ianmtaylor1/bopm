# c.bopm - wrap the primitive c for bopm classes where we
# preserve all the extra attributes
#' @export
c.bopm <- function(...) {
  # Check all attributes against each other. Should be equal.
  first <- NULL
  for (obj in list(...)) {
    if (is.null(first)) {
      first <- obj
    } else {
      attrmatch <- attr.all.equal(first, obj)
      if (!is.null(attrmatch)) {
        stop("Attributes of arguments don't match: '", attrmatch, "'")
      }
    }
  }
  # Cast list as mcmc.list to catch if there are different timescales, 
  # variables, etc
  combined <- coda::mcmc.list(NextMethod())
  attributes(combined) <- attributes(first)
  return(combined)
}
