# Make predictions using a bopm model
#' @export
predict.bopm <- function(model, newdata=NULL, ...) {
  # Create the x matrix we are using for predictions
  if (is.null(newdata)) {
    newdata <- attr(model, "build.data")
  }
  xmat <- model.matrix(attr(model, "formula"), newdata)
  # Turn posterior samples into matrix
  post.samples <- as.matrix(model)
  # Get intermediate "latent" response for each x (row), each posterior sample (column)
  latent <- xmat %*% t(post.samples[,colnames(xmat)])
  # Find the probability of the response being in each category
  ncat <- attr(model, "ncat")
  probs <- matrix(0, nrow=dim(xmat)[1], ncol=ncat)
  colnames(probs) <- 1:ncat
  for (i in 1:ncat) {
    # Find the lower bound for this category for each sample
    lbname <- paste(attr(model, "threshold.prefix"), i-1, sep="")
    if (i == 1) {
      lb <- -Inf
    } else if (lbname %in% colnames(post.samples)) {
      lb <- post.samples[,lbname]
    } else {
      lb <- 0
    }
    # Find the upper bound for this category for each sample
    ubname <- paste(attr(model, "threshold.prefix"), i, sep="")
    if (i == ncat) {
      ub <- Inf
    } else if (ubname %in% colnames(post.samples)) {
      ub <- post.samples[,ubname]
    } else {
      ub <- 0
    }
    # Calculate the probability for this category
    allprobs <- t(pnorm(q=ub, mean=t(latent)) - pnorm(q=lb, mean=t(latent)))
    probs[,i] <- rowMeans(allprobs)
  }
  return(probs)
}
