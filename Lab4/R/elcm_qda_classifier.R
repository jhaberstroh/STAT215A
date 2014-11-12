library('Rcpp')
library('mclust')
library('MASS')
sourceCpp('elcm_threshold.cpp')


FindMinNDAI <- function(image, labels, search.NDAI, threshold.corr, threshold.sd)
{
  err.NDAI <- rep(0, length(search.NDAI))
  for (i in 1:length(search.NDAI)) {
    err.NDAI[i] <- ClassificationError(image$SD, threshold.sd, TRUE,
                                       image$CORR, threshold.corr, FALSE,
                                       image$NDAI, search.NDAI[i], TRUE,
                                       labels)  
  }
  plot(search.NDAI, err.NDAI)
  return(search.NDAI[which.min(err.NDAI)])
}

TrainELCMQDA <- function(data, labels, baseline_param=NULL, threshold.sd=2, threshold.corr=.075)
{
  if (is.null(baseline_param))
  {   
    print(threshold.sd)
    print(threshold.corr)
    search.NDAI <- seq(0,2,0.001)
    ndai.min <- FindMinNDAI(data, labels, search.NDAI, threshold.corr, threshold.sd)
    baseline_parameters <- vector(mode="list", length=3)
    names(baseline_parameters) <- c("thresh.NDAI", "thresh.SD", "thresh.CORR")
    baseline_parameters$thresh.NDAI <- ndai.min
    baseline_parameters$thresh.SD <- threshold.sd
    baseline_parameters$thresh.CORR <- threshold.corr  
  }
  parameters <- vector(mode="list", length=3)
  names(parameters) <- c("thresh.NDAI", "thresh.SD", "thresh.CORR")
  parameters$thresh.SD <- baseline_parameters$thresh.SD
  parameters$thresh.CORR <- baseline_parameters$thresh.CORR
  # Perform the mixture model to find the EM-thresholded NDAI value
  ndai.mixture = Mclust(data$NDAI, G = 2, modelNames = c("E", "V"))
  ndai.mixture.mean <- ndai.mixture$parameters$mean
  ndai.mixture.var <- ndai.mixture$parameters$variance$sigmasq
  ndai.range = range(image.one$NDAI)
  ndai.points.sampled = seq(ndai.range[1], ndai.range[2], 0.0001)
  NormalDist <- function(x,mean,var) (1/sqrt(2*pi*var)*exp(-(x-mean)^2/var))
  ndai.mixture.sampled <- NormalDist(ndai.points.sampled, ndai.mixture.mean[1], ndai.mixture.var[1]) + 
                          NormalDist(ndai.points.sampled, ndai.mixture.mean[2], ndai.mixture.var[2]) 
  second.deriv.sign <- diff(sign(diff(ndai.mixture.sampled)))
  min.index <- which(second.deriv.sign == 2)
  # Use the EM result if we found a minimum index!
  if (min.index != 0)
  {
    # We must add one to account for numerical differentiation
    parameters$thresh.NDAI <- ndai.points.sampled[min.index + 1]
  }
  else
  {
    parameters$thresh.NDAI <- baseline_parameters$thresh.NDAI
  }
  return(parameters)
}
  

CostELCMQDA <- function(data, labels, parameters)
{
  head(data$SD)
  thresh.predict <- data$SD   < parameters$thresh.SD | 
                    (data$CORR > parameters$thresh.CORR & 
                      data$NDAI < parameters$thresh.NDAI )
  thresh.predict <- (thresh.predict * -2) + 1
  
  qda.predict <- qda(thresh.predict ~ SD + CORR + NDAI,
                     data = data, CV = TRUE)
  #qda.prob.nocloud <- qda2$posterior[,1]
  qda.predict.error <- qda.predict$class != labels
  qda.err <- mean(qda.predict.error[labels!=0])
  print(1-qda.err)  
  return(qda.predict$class)
}

