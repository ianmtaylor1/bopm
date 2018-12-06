# Bayesian Ordinal Probit Model
# formula - the model formula to be fitted
# data - a data.frame containing the data necessary to fit the model
# ncat - Number of possible categories for response (default: max(y))
# beta.mean - prior mean for beta (regression coefficients) (default: zero)
# beta.covar - prior covariance matrix for beta (regression coefficients) (default: identity)
# threshold.scale - prior standard deviation of category thresholds
# n.iter - number of iterations per chain
# n.chains - number of chains to run
# threshold.prefix - prefix to use naming the thresholds between categories
# print - if TRUE, print small progress tracking update
# Returns: a coda mcmc.list object with all iterations
bopm <- function(formula, data, ncat=NULL,
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

  chain.list <- list()
  for(chain in 1:n.chains) {

    if (print == TRUE) {
      cat("Chain ",chain,": (%) ",sep="")
    }

    # Initial values of parameters:
    # beta - regression coefficients
    beta <- c(rmvnorm(n=1, mu=beta.mean, Sigma=beta.covar))
    # sep - Separators between categories
    sep <- rep(0, ncat - 1)
    dstnct <- floor((ncat - 1) / 2)
    if (dstnct > 0) {
      sep[1:dstnct] <- sort(rtruncnorm(n=dstnct, sd=threshold.scale, b=0))
      sep <- sep - rev(sep)
    }

    # Create data frame to hold posterior samples
    results <- data.frame(matrix(0, nrow=n.iter, ncol=p+ncat-1))
    colnames(results) <- c(colnames(xmat), paste(threshold.prefix, 1:(ncat-1), sep=""))

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
      sep <- update_sep_fc(y, latent, ncat, threshold.scale)

      # Update beta
      beta <- update_beta_fc(latent, xmat, beta.mean, beta.covar,
                             sigma.inv=beta.covar.inv, post.var=beta.fc.var,
                             post.var.chol=beta.fc.var.chol)

      # Save the current state
      results[rep,] <- c(beta,sep)
    }

    # Add this chain to the results
    chain.list[[chain]] <- as.mcmc(results)

    if (print == TRUE) {
      cat("\n")
    }
  }

  # Construct object to return
  results <- mcmc.list(chain.list)
  attr(results, "formula") <- formula
  attr(results, "ncat") <- ncat
  attr(results, "threshold.prefix") <- threshold.prefix
  attr(results, "build.data") <- data
  class(results) <- c("bopm", class(results))

  return(results)
}
