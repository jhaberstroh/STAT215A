library(dplyr)
library(ggplot2)
library('Rcpp')

setwd(file.path(Sys.getenv("GIT_REPO_LOC"), "Users/cusgadmin/Documents/2014fall/STAT215/Lab/Lab4/image_data"))
# Get the data for three images

image.one <- read.table('image1.txt', header=F)
image.two <- read.table('image2.txt', header=F)
image.three <- read.table('image3.txt', header=F)

# Add informative column names.
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image.one) <- collabs
names(image.two) <- collabs
names(image.three) <- collabs

head(image.one)
summary(image.one)

head(image.two)
head(image.three)


# Class conditional densities.
ggplot(image1) + geom_density(aes(x=SD, group=factor(label), fill=factor(label)), alpha=0.5) +
  ggtitle('image1')

ggplot(image.one) + geom_point(aes(x=x, y=y, color=AN))
# The classification.
ggplot(image.one) + geom_point(aes(x=x, y=y, color=factor(label)))
# Class conditional densities.
ggplot(image.one) + geom_density(aes(x=AN, group=factor(label), fill=factor(label)), alpha=0.5)
ggplot(image.one) + geom_density(aes(x=CORR, group=factor(label), fill=factor(label)), alpha=0.5)
ggplot(image.one) + geom_density(aes(x=NDAI, group=factor(label), fill=factor(label)), alpha=0.5)


## Realization of Enhanced Linear Correlation Matching Algorithm
# ELCM: setting threshold for features
setwd(file.path(Sys.getenv("GIT_REPO_LOC"), "Users/cusgadmin/Documents/2014fall/STAT215/Lab/Lab4"))
source('exploration_func.R')
threshold.corr <- 0.75
threshold.sd <- 2
image.one.sample <- image.one[sample.int(nrow(image.one), 10000, replace=FALSE),]
# Classify using cutoff on NDAI & SD first, leaving behind misclassified entries
# initialize predicted label as 1
image.one$label.pre <- 1
image.one$label.pre[image.one$SD<threshold.sd | (image.one$CORR>threshold.corr & image.one$NDAI < 0.5)] <- -1


class.error <- function(label.true, label.pre) {
  return (sum(label.true[label.true != 0] != label.pre[label.true != 0])/sum(label.true != 0))
}
err <- class.error(image.one$label, image.one$label.pre)

search.NDAI <- seq(0,1,0.00001)
err.NDAI <- rep(0, length(search.NDAI))
for (i in 1:length(search.NDAI)) {
  image.one$label.pre <- 1
  image.one$label.pre[image.one$SD<threshold.sd | (image.one$CORR>threshold.corr & image.one$NDAI < search.NDAI[i])] <- -1
  err.NDAI[i] <- class.error(image.one$label, image.one$label.pre)
  print(i)
}


ggplot(image.one, aes(x = NDAI)) + geom_histogram()


library('mclust')
## image.one
mod1 = Mclust(image.one$NDAI, G = 2, modelNames = c("E", "V"))
summary(mod1,parameters = TRUE)

gaussian.mean <- mod1$parameters$mean
gaussian.var <- mod1$parameters$variance$sigmasq

## Generate Gaussian function
gaussian <- function(x, mean, var) {
  return (1/sqrt(2*pi*var)*exp(-(x-mean)^2/var))
}

x = range(image.one$NDAI)
x = seq(x[1], x[2], 0.0001)
mix <- gaussian(x, gaussian.mean[1], gaussian.var[1]) + gaussian(x, gaussian.mean[2], gaussian.var[2]) 
second.deriv <- diff(sign(diff(mix)))
dip <- x[which.max(second.deriv)]

## image.two
mod2 = Mclust(image.two$NDAI, G = 2, modelNames = c("E", "V"))
summary(mod2,parameters = TRUE)

gaussian.mean <- mod2$parameters$mean
gaussian.var <- mod2$parameters$variance$sigmasq

x = range(image.two$NDAI)
x = seq(x[1], x[2], 0.0001)
mix <- gaussian(x, gaussian.mean[1], gaussian.var[1]) + gaussian(x, gaussian.mean[2], gaussian.var[2]) 
second.deriv <- diff(sign(diff(mix)))
dip <- x[which.max(second.deriv)]

image.two$label.pre = 1
image.two$label.pre[image.two$SD<threshold.sd | (image.two$CORR>threshold.corr & image.two$NDAI < dip )] <- -1
err.two <- class.error(image.two$label, image.two$label.pre)

## QDA
library("MASS")
qda2 <- qda(label.pre~SD+CORR+NDAI, data = image.two, CV = TRUE)
image.two$nocloud.prob <- qda2$posterior[,1]
image.two$label.pre.qda <- qda2$class
err.two.qda <- class.error(image.two$label, image.two$label.pre.qda)
ggplot(image.two) + geom_point(aes(x=x, y=y, color=label))
ggplot(image.two) + geom_point(aes(x=x, y=y, color=nocloud.prob))
ggplot(image.two) + geom_point(aes(x=x, y=y, color=label.pre.qda))
ggplot(image.two) + geom_point(aes(x=x, y=y, color=label.pre))
