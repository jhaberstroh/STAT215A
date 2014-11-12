library(dplyr)
library(ggplot2)
sourceCpp('crf_classifier_cpp.cpp')
source('elcm_qda_classifier.R')

crf_classifier_cv <- function(image.train, image, iterationNum, beta) {
  params <- TrainELCMQDA(image.train, image.train$label)
  thresh.predict <- image$SD < params$thresh.SD | 
    (image$CORR > params$thresh.CORR & 
       image$NDAI < params$thresh.NDAI )
  thresh.predict <- (thresh.predict * -2) + 1  
  qda.predict <- qda(thresh.predict ~ SD + CORR + NDAI,
                     data = image, CV = TRUE) 
  image$label.temp <- qda.predict$class  
  image$posterior.cloud <- qda.predict$posterior[,2]
  
  # Consider this problem in the framework of conditional random field,
  # we are basically trying to maximize the likelihood, 
  # and take into account the spatial homogeneity at the same time
  # Iterative conditional modes algorithm is used to solve 
  # the optimization problem
  image$label.pre <- CrfClassifier_Posterior (iterationNum, beta, 
                                                image$x, image$y,
                                                image$label,
                                                image$label.temp, image$posterior.cloud)
  
  return (image$label.pre)
}
