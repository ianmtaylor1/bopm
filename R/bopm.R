# Bayesian Ordinal Probit Model
# formula - the model formula to be fitted
# data - a data.frame containing the data necessary to fit the model
# ncat - Number of possible categories for response (default: max(y))
# symetric - whether the thresholds should be symmetric around zero
# beta.mean - prior mean for beta (regression coefficients) (default: zero)
# beta.covar - prior covariance matrix for beta (regression coefficients) (default: identity)
# threshold.scale - prior standard deviation of category thresholds
# n.iter - number of iterations per chain
# n.chains - number of chains to run
# threshold.prefix - prefix to use naming the thresholds between categories
# print - if TRUE, print small progress tracking update
# Returns: a coda mcmc.list object with all iterations
#' @export
bopm <- function(formula, data, ncat=NULL, symmetric=FALSE,
                 beta.mean=NULL, beta.covar=NULL, threshold.scale=1,
                 n.iter=10000, n.chains=2,
                 threshold.prefix="_theta_",
                 print=FALSE
) {
  # Get design matrix, response vector and number of categories
  stopifnot(length(formula) == 3) # Formula must have LHS
  mf <- model.frame(formula, data, na.action=na.fail)
  y <- c(mf[,1])
  xmat <- model.matrix(formula, mf)
  if (is.null(ncat)) ncat <- max(y)

  # Make sure the y vector looks like we want it to
  stopifnot(y == floor(y), y >= 1, y <= ncat)

  # How much data and how many parameters do we have?
  n <- length(y)
  p <- dim(xmat)[2]

  # Fill in default prior means and covariances
  if (is.null(beta.mean)) beta.mean <- rep(0,p)
  if (is.null(beta.covar)) beta.covar <- diag(p)
  stopifnot(length(beta.mean) == p)
  stopifnot(dim(beta.covar) == c(p,p))

  # Pre-compute difficult matrix inverses, etc.
  beta.covar.inv <- chol2inv(chol(beta.covar))
  beta.fc.var <- chol2inv(chol(t(xmat) %*% xmat + beta.covar.inv))
  beta.fc.var.chol <- chol(beta.fc.var)

  # Which separators do we not need to save?
  zerosepname <- NULL
  if (symmetric && ((nsep %% 2) == 0)) {
    zerosepname <- paste(threshold.prefix, nsep/2, sep="")
  } else if (!symmetric) {
    zerosepname <- paste(threshold.prefix, 1, sep="")
  }

  # Run each chain
  chain.list <- list()
  for(chain in 1:n.chains) {

    if (print == TRUE) {
      cat("Chain ",chain,": (%) ",sep="")
    }

    # Initial values of parameters:
    # beta - regression coefficients
    beta <- c(rmvnorm(n=1, mu=beta.mean, Sigma=beta.covar))
    # sep - Separators between categories
    # Use regular fc update with minimal "dummy" data
    sep <- update_sep_fc(y=c(1, ncat), z=c(-Inf, Inf), ncat, threshold.scale, symmetric)

    # Create data frame to hold posterior samples
    chainsamples <- data.frame(matrix(0, nrow=n.iter, ncol=p+ncat-1))
    colnames(chainsamples) <- c(colnames(xmat), paste(threshold.prefix, 1:(ncat-1), sep=""))

    # MCMC/Gibbs sampling
    for(rep in 1:n.iter) {

      if ((print == TRUE) && (rep %% (n.iter %/% 50) == 0)) {
        if ((rep %% (n.iter %/% 10)) == 0) {
          cat(rep %/% (n.iter %/% 100))
        } else {
          cat(".")
        }
      }

      # Update latent variables
      latent <- update_latent_fc(y, xmat, beta, sep)

      # Update separators
      sep <- update_sep_fc(y, latent, ncat, threshold.scale, symmetric)

      # Update beta
      beta <- update_beta_fc(latent, xmat, beta.mean, beta.covar,
                             sigma.inv=beta.covar.inv, post.var=beta.fc.var,
                             post.var.chol=beta.fc.var.chol)

      # Save the current state
      chainsamples[rep,] <- c(beta,sep)
    }

    # Remove the nuissance zero separator
    if (!is.null(zerosepname)) {
      chainsamples[[zerosepname]] <- NULL
    }

    # Add this chain to the results
    chain.list[[chain]] <- coda::as.mcmc(chainsamples)

    if (print == TRUE) {
      cat("\n")
    }
  }

  # Construct object to return
  results <- coda::mcmc.list(chain.list)
  # Make it a bopm class and add attributes from function call
  class(results) <- c("bopm", class(results))
  attr(results, "formula") <- formula
  attr(results, "build.data") <- data
  attr(results, "ncat") <- ncat
  attr(results, "symmetric") <- symmetric
  attr(results, "beta.mean") <- beta.mean
  attr(results, "beta.covar") <- beta.covar
  attr(results, "threshold.scale") <- threshold.scale
  attr(results, "threshold.prefix") <- threshold.prefix
  # Don't need: n.iter, n.chains, print

  return(results)
}
