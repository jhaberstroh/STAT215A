source('elcm_qda_classifier.R')


CostIsingPostCorrELCM <- function(data, labels, parameters, lambda=.1)
{
  head(data)
  head(data$SD)
  thresh.predict <- data$SD   < parameters$thresh.SD | 
    (data$CORR > parameters$thresh.CORR & 
       data$NDAI < parameters$thresh.NDAI )
  thresh.predict <- (thresh.predict * -2) + 1
  
  qda.predict <- qda(thresh.predict ~ SD + CORR + NDAI,
                     data = data, CV = TRUE)
  
  correction.class <- IsingPert(data, qda.predict$posterior)
  qda.predict.error <- correction.class != labels
  qda.err <- mean(qda.predict.error[labels!=0])
  
  return(qda.err)
}


# 
# library(Rcpp)
# sourceCpp(paste0(wd, "/", "ising.cpp"))
# image.one.extent <- c(min(image.one$x), max(image.one$x), min(image.one$y), max(image.one$y))
# Jmtx <- LearnJ(image.one$x, image.one$y, image.one.extent, image.one$label)
