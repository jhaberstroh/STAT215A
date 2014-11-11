library(dplyr)
library(ggplot2)
library(Rcpp)

sourceCpp('classifier.cpp')
FindMinNDAI <- function(image, search.NDAI, threshold.corr, threshold.sd)
{
  err.NDAI <- rep(0, length(search.NDAI))
  for (i in 1:length(search.NDAI)) {
    err.NDAI[i] <- ClassificationError(image$SD, threshold.sd, TRUE,
                                       image$CORR, threshold.corr, FALSE,
                                       image$NDAI, search.NDAI[i], TRUE,
                                       image$label)  
  }
  plot(search.NDAI, err.NDAI)
  return(search.NDAI[which.min(err.NDAI)])
}


image.one <- read.table('data/image1.txt', header=F)
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image.one) <- collabs

search.NDAI <- seq(0,2,0.001)
ndai.min <- FindMinNDAI(image.one, search.NDAI, .075, 2)
ndai.min
