# window.bopm - wrap the window method of a coda::mcmc.list but
# preserve the extra attributes of a bopm.
#' @export
window.bopm <- function(object, ...) {
  attrs <- attributes(object)
  w <- NextMethod() # Calls the window.mcmc.list method
  attributes(w) <- attrs # Includes class automatically
  return(w)
}
