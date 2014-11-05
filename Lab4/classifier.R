library(dplyr)
library(ggplot2)
source('elcm_qda_classifier.R')

setwd(file.path(Sys.getenv("GIT_REPO_LOC"), "Users/cusgadmin/Documents/2014fall/STAT215/Lab/Lab4_repo/Lab4"))

# Get the data for three images

# New classifier code:
image.one <- read.table('image1.txt', header=F)
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image.one) <- collabs
params <- TrainELCMQDA(image.one, image.one$label)
CostELCMQDA(image.one, image.one$label, params)

label.pre <- crf_classifier(image.one,5, 0.2)
