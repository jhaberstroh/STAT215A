library(dplyr)
library(ggplot2)
library(Rcpp)

sourceCpp('classifier.cpp')

image.one <- read.table('data/image1.txt', header=F)
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image.one) <- collabs

threshold.corr <- 0.75
threshold.sd <- 2
search.NDAI <- seq(0,1,0.00001)
# err.NDAI <- rep(0, length(search.NDAI))
# 
# for (i in 1:length(search.NDAI)) {
#   print(i)
#   err.NDAI[i] <- ClassificationError(image.one$SD, threshold.sd, TRUE,
# 				     image.one$CORR, threshold.corr, FALSE,
# 				     image.one$NDAI, search.NDAI[i], TRUE,
# 				     image.one$label)
# }

err.NDAI <- ClassificationSearch_w2thresh(image.one$SD, threshold.sd, TRUE,
                                          image.one$CORR, threshold.corr, FALSE,
                                          image.one$NDAI, search.NDAI, TRUE,
                                          image.one$label, verbose=TRUE)
