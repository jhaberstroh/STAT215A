
CheckPointMixtureArgs <- function(p, means, sds) {
  if (length(means) != length(p)) {
    stop("Means and p must have the same length.")
  }
  if (length(sds) != length(p)) {
    stop("sds and p must have the same length.")
  }
  if (any(p < 0 | p > 1)) {
    stop("Bad p.")
  }
}

NormalPointMixtureDraws <- function(n, p, means, sds) {
  # Generate draws from a point mixture of normals.
  # Args:
  #   n: The number of data points.
  #   p: The probabilities of each component.  Should sum to one.
  #   means: The means of each component.  Should have length(means) == length(p)
  #   sds: The standard deviation of each component.  Should have lenght(sds) == length(p)
  #
  # Returns:
  # Draws from a point mixture of normals.
  CheckPointMixtureArgs(p, means, sds)
  
  # Generate component labels.  Note that you need to reverse the p
  # direction because of how cumsum works.
  ids <- apply(apply(rmultinom(n, 1, rev(p)), 2, cumsum), 2, sum)
  ids[ids == 0] <- 1  # This should not happen, but just in case of edge cases.
  
  # Generate normals.
  return(rnorm(n, mean=means[ids], sd=sds[ids]))
}

NormalPointMixtureDensity <- function(x, p, means, sds) {
  # Evaluate a normal point mixture PDF.
  # Args:
  #   x: The points at which to evaluate the density.
  #   Other args the same as NormalPointMixtureDraws.
  # Returns:
  #   The pdf evaluated at x.
  CheckPointMixtureArgs(p, means, sds)
  k  <- length(p)
  pdf.vals <- do.call(cbind, lapply(1:k, function(i) { dnorm(x, means[i], sds[i])}))
  return(pdf.vals %*% p)
}

