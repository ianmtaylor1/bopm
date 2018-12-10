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
      # attr.all.equal returns NULL if objects' attributes are all equal
      stopifnot(is.null(attr.all.equal(first, obj)))
    }
  }
  combined <- NextMethod()
  attributes(combined) <- attributes(first)
  return(combined)
}
