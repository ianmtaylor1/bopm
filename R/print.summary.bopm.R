# Method to print the summary of a bopm object
#' @export
print.summary.bopm <- function(object, ...) {
  cat("Bayesian Ordinal Probit Model\n\n")

  # Print the parts specific to bopm
  cat("Model Formula: ")
  print(object$formula)
  cat("\nSample Size: ", object$n, "\n", sep="")
  cat("\nCateogry Counts:\n")
  cats <- object$ystats
  names(cats) <- 1:length(object$ystats)
  print(cats)
  if (object$symmetric) {
    cat("Symmetric\n")
  } else {
    cat("Non-symmetric\n")
  }

  # Print the summary of the coda::mcmc.list components
  cat("\n--- MCMC Summary: ---\n")
  NextMethod()
  invisible(object)
}
