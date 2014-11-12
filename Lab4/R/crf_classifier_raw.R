library(dplyr)
library(ggplot2)
source('elcm_qda_classifier.R')

sourceCpp('crf_classifier_cpp.cpp')

# Get the data for three images

# New classifier code:
image.one <- read.table('image1.txt', header=F)
image.two <- read.table('image2.txt', header=F)
image.three <- read.table('image3.txt', header=F)
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image.one) <- collabs
names(image.two) <- collabs
names(image.three) <- collabs
params <- TrainELCMQDA(image.one, image.one$label)
CostELCMQDA(image.one, image.one$label, params)

ggplot(image.three) + geom_density(aes(x=DF, group=factor(label), fill=factor(label)), alpha=0.5)



## Classifier considering local homogeneity using markov random field 

data <- image.one
ndai.mixture = Mclust(image.one$NDAI, G = 2, modelNames = c("E", "V"))
ndai.mixture.mean <- ndai.mixture$parameters$mean
names(ndai.mixture.mean) <- NULL
ndai.mixture.var <- ndai.mixture$parameters$variance$sigmasq
parameters <- params
thresh.predict <- data$SD < parameters$thresh.SD | 
  (data$CORR > parameters$thresh.CORR & 
     data$NDAI < parameters$thresh.NDAI )
thresh.predict <- (thresh.predict * -2) + 1

qda.predict <- qda(thresh.predict ~ SD + CORR + NDAI,
                   data = data, CV = TRUE)
image.one$label.qda <- qda.predict$class
image.one$label.temp <- qda.predict$class

# convert the dataframe to matrix


# formal code goes here
iteration.num <- 2
pixel.num <- 100
for (k in (1:iteration.num)) {
  for (i in (1:pixel.num)) {
    print(i)
    image.one[i,"label.temp"] <- 1
    post.energy1 <- SingletonPotential(i, image.one[i,"NDAI"], 1, 
                                       ndai.mixture.mean, ndai.mixture.var) +
      CliquePotential(image.one, i)
    image.one[i,"label.temp"] <- -1
    post.energy2 <- SingletonPotential(i, image.one[i,"NDAI"], 1,
                                       ndai.mixture.mean, ndai.mixture.var) +
      CliquePotential(image.one, i)
    if (post.energy1 < post.energy2) {
      image.one[i,"label.temp"] <- 1
    }else {
      image.one[i,"label.temp"] <- -1
    }
  }
}

crf.predict.error <- image.one$label.temp != image.one$label
crf.err <- mean(crf.predict.error[image.one$label!=0])

ggplot(image.one) + geom_point(aes(x=x, y=y, color=factor(label)))
ggplot(image.one) + geom_point(aes(x=x, y=y, color=factor(label.qda)))
ggplot(image.one) + geom_point(aes(x=x, y=y, color=factor(label.temp)))


SingletonPotential <- function(pixel.ind, y, label, mean, var) {
  if (label == 1) {
    V <- log(sqrt(2*pi*var[2])) + 
      0.5*(y - mean[2])^2
  } else {
    V <- log(sqrt(2*pi*var[1])) + 
      0.5*(y - mean[1])^2
  }
  return (V)
}

CliquePotential <- function(image, pixel.ind) {
  # pixel.ind is the row index of current pixel
  V <- 0
  if (!(image[pixel.ind,"x"] == range(image$x)[1])){
    x <- (image[pixel.ind, "label.temp"] != 
            image[image$x == (image[pixel.ind,"x"]-1) &
                    image$y == image[pixel.ind,"y"],"label.temp"])*2-1
    if (length(x)) {
      V <- V+x
    }
  }
  if (!(image[pixel.ind,"x"] == range(image$x)[2])) { 
    x <-  (image[pixel.ind, "label.temp"] != 
             image[image$x == (image[pixel.ind,"x"]+1) &
                     image$y == image[pixel.ind,"y"],"label.temp"])*2-1
    if (length(x)) {
      V <- V+x
    }
  }
  if (!(image[pixel.ind,"y"] == range(image$y)[1])) {
    x <-  (image[pixel.ind, "label.temp"] != 
             image[image$y == (image[pixel.ind,"y"] + 1) &
                     image$x == image[pixel.ind,"x"],"label.temp"])*2-1
    if (length(x)) {
      V <- V+x
    }
  }
  if (!(image[pixel.ind,"y"] == range(image$y)[2])) {
    x <- (image[pixel.ind, "label.temp"] != 
            image[(image$y == image[pixel.ind,"y"] - 1) &
                    image$x == image[pixel.ind,"x"],"label.temp"])*2-1
    if (length(x)) {
      V <- V+x
    }
  }
  return (V)
}

## Realize the crf algorithm using c++
params <- TrainELCMQDA(image.one, image.one$label)
CostELCMQDA(image.one, image.one$label, params)
ndai.mixture = Mclust(image.one$NDAI, G = 2, modelNames = c("E", "V"))
ndai.mixture.mean <- ndai.mixture$parameters$mean
names(ndai.mixture.mean) <- NULL
ndai.mixture.var <- ndai.mixture$parameters$variance$sigmasq
data <- image.one
parameters <- params
thresh.predict <- data$SD < parameters$thresh.SD | 
  (data$CORR > parameters$thresh.CORR & 
     data$NDAI < parameters$thresh.NDAI )
thresh.predict <- (thresh.predict * -2) + 1

qda.predict <- qda(thresh.predict ~ SD + CORR + NDAI,
                   data = data, CV = TRUE)
data$label.qda <- qda.predict$class

data$label.temp <- qda.predict$class

data$post.cloud <- qda.predict$posterior[,2]

err.crf.cpp.post <- CrfClassifier_Posterior (5,0.1, data$x, data$y,
                              data$label,
                              data$label.temp, data$post.cloud)
ggplot(data) + geom_point(aes(x=x, y=y, color=as.numeric(err.crf.cpp.post)))
ggplot(data) + geom_point(aes(x=x, y=y, color=label.temp))

data <- image.two
params <- TrainELCMQDA(data, data$label)
CostELCMQDA(data, data$label, params)
parameters <- params
thresh.predict <- data$SD < parameters$thresh.SD | 
  (data$CORR > parameters$thresh.CORR & 
     data$NDAI < parameters$thresh.NDAI )
thresh.predict <- (thresh.predict * -2) + 1

qda.predict <- qda(thresh.predict ~ SD + CORR + NDAI,
                   data = data, CV = TRUE)
data$label.qda <- qda.predict$class

data$label.temp <- qda.predict$class

data$post.cloud <- qda.predict$posterior[,2]

err.crf.cpp.post <- CrfClassifier_Posterior (5,0.1, data$x, data$y,
                                             data$label,
                                             data$label.temp, data$post.cloud)
ggplot(data) + geom_point(aes(x=x, y=y, color=as.numeric(err.crf.cpp.post)))
ggplot(data) + geom_point(aes(x=x, y=y, color=label.temp))

data <- image.three
params <- TrainELCMQDA(data, data$label)
CostELCMQDA(data, data$label, params)
parameters <- params
thresh.predict <- data$SD < parameters$thresh.SD | 
  (data$CORR > parameters$thresh.CORR & 
     data$NDAI < parameters$thresh.NDAI )
thresh.predict <- (thresh.predict * -2) + 1

qda.predict <- qda(thresh.predict ~ SD + CORR + NDAI,
                   data = data, CV = TRUE)
data$label.qda <- qda.predict$class

data$label.temp <- qda.predict$class

data$post.cloud <- qda.predict$posterior[,2]

err.crf.cpp.post <- CrfClassifier_Posterior (5,0.1, data$x, data$y,
                                             data$label,
                                             data$label.temp, data$post.cloud)
ggplot(data) + geom_point(aes(x=x, y=y, color=as.numeric(err.crf.cpp.post)))
ggplot(data) + geom_point(aes(x=x, y=y, color=label.temp))
ggplot(data) + geom_point(aes(x=x, y=y, color=label))
