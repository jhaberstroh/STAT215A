
# Fill in a kernel function
# Could be Gaussian, square, cosine
Kernel <- function(x, h) {
  # A kernel function for use in nonparametric estimation.
  # Args:
  #  x: The point to evaluate the kernel
  #  h: The bandwidth of the kernel.
  # Returns:
  #  The value of a kernel with bandwidth h evaluated at x.
  BaseKernel <- function(x) {
    return(abs(x) < 0.5)
  }
  return((1 / h) * BaseKernel(x / h))
}

EstimateDensity <- function(x.data, KernelFun, h, resolution=length(eval.x), eval.x=NULL) {
  # Perform a kernel density estimate.
  # Args:
  #   x.data: The observations from the density to be estimated.
  #   KernelFun: A kernel function.
  #   h: the bandwidth.
  #   resolution: The number of points at which to evaluate the density.  Only necessary
  #               if eval.x is unspecified.
  #   eval.x: Optional, the points at which to evaluate the density.  Defaults to
  #           resolution points in [ min(x.data), max(x.data) ]
  # Returns:
  #  A data frame containing the x values and kernel density estimates with
  #  column names "x" and "f.hat" respectively.
  if (!is.null(eval.x)){
    result <- data.frame(x=eval.x)
    result$f.hat <- sapply(result$x, function(x) sum(KernelFun(x.data-x,h)))
    result <- transform(result, f.hat = f.hat/length(x.data))
    return(result)
  }
  else{
    return(0)
  }
}


PerformSimulations <- function(sims, n, p, means, sds, eval.x, KernelFun, h) {
  # Simulate data from a normal point mixture and perform kernel estimators.
  # Args:
  #  sims: The number of simulations to run.
  #  n: The number of points in the dataset.
  #  p: The probabilities of each mixture component.
  #  means: The means of each mixture component.
  #  sds: The standard deviations of each mixture component.
  #  eval.x: The points at which to estimate the density.
  #  KernelFun: The kernel function, which should take a point and a bandwidth as arguments.
  #  h: The bandwidth of the estimator.
  #
  # Returns:
  #  A data frame containing the following information:
  #    sim: Which simulation the data came from
  #    x: The points at which the density is estimated.
  #    f.hat: The estimated density at x for this simulation.
  #    true.pdf: The true pdf at x
  
  for (i in seq(1,sims)){
    result.sim <- data.frame()
    data <- NormalPointMixtureDraws(n,p,means,sds)
    density.estimate <- EstimateDensity(data, KernelFun, h, eval.x=eval.x)
    result.sim <- density.estimate
    ## WORKING ERROR
    result.sim$sim <- i
    result.sim$true.pdf <- NormalPointMixtureDensity(result.sim$x, p, means, sds)
    return(result.sim)
    if (nrow(result) == 0){
      result <- result.sim
    }
    else{
      result <- rbind(result, result.sim)      
    }
  }
  
  return(result)
}
