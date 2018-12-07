# window.bopm - wrap the window method of a coda::mcmc.list but
# preserve the extra attributes of a bopm.
window.bopm <- function(object, ...) {
  attrs <- attributes(object)
  w <- NextMethod()
  attributes(w) <- attrs # Includes class automatically
  return(w)
}
