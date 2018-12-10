# Bayesian Ordinal Probit Regression (Parallel)
# Identical to the 'bopm' function, expcept splits the execution of each chain
# to a parallel process. Must be run
# cluster - a culster (created with parallel::makeCluster) over which to spread the chains
#' @export
bopm.parallel <- function(cluster, n.chains=2, ...) {
  # Check for 
  if (!requireNamespace("parallel", quietly=TRUE)) {
    stop("Package 'parallel' required")
  }
  # Run chains in load balanced way on cluster, passing through all arguments
  # passed to this function (except n.chains, which we set to 1)
  reslist <- parallel::clusterApplyLB(cl=cluster, x=1:n.chains,
                                      fun=function(x, ...) bopm(n.chains=1, ...),
                                      ...)
  # Recombine them into one bopm object, and return it.
  do.call(c, reslist)
}