# Symmetric Ordinal Linear Regression
# y - The ordinal response variable. Should take integer values >= 1
# X - data.frame with covariates
# ncat - Number of possible categories for response
# beta.mean - prior mean for beta (regression coefficients)
# beta.covar - prior covariance matrix for beta (regression coefficients)
# n.iter - number of iterations per chain
# n.chains - number of chains to run
# print - if TRUE, print small progress notification
# Returns: a coda mcmc.list object with all iterations
solr <- function(y, X, ncat=max(y),
                 beta.mean=rep(0, dim(X)[2]), beta.covar=diag(dim(X)[2]),
                 n.iter=10000, n.chains=2,
                 print=FALSE
) {
  # Make sure the y vector looks like we want it to
  stopifnot(y == floor(y), y >= 1, y <= ncat)
  # Make sure X looks like we want it to
  stopifnot(is.data.frame(X))
  stopifnot(dim(X)[1] == length(y))
  xmat <- data.matrix(X)

  # How much data and how many parameters do we have?
  n <- length(y)
  p <- dim(X)[2]

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
      sep[1:dstnct] <- sort(rtruncnorm(n=dstnct, b=0))
      sep <- sep - rev(sep)
    }

    # Create data frame to hold posterior samples
    results <- data.frame(matrix(0, nrow=n.iter, ncol=p+ncat-1))
    colnames(results) <- c(colnames(X), paste("sep", 1:(ncat-1), sep=""))

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
      sep <- update_sep_fc(y, latent, ncat)

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

  return(mcmc.list(chain.list))
}
