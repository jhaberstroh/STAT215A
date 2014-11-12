library(optparse)

option_list <- list(make_option(c("-c","--config"), default="cfg.yml", help="Location of .yml configuration file"))
parser <- OptionParser(usage = "Hello", option_list = option_list)
args = parse_args(parser)
print(args$config)

library(dplyr)
library(ggplot2)
<<<<<<< HEAD:Lab4/classifier.R


=======
library(yaml)
yaml <- yaml.load_file(args$config)
print(yaml$config$dir)
print(yaml$config$dir)

source(paste0(yaml$config$dir,'/R/','elcm_qda_classifier.R'))
source(paste0(yaml$config$dir,'/R/','crf_classifier.R'))
>>>>>>> d94d2e3a9c86447794c2ff202cb06480383d799a:Lab4/scripts/crf_example.R

setwd(file.path(Sys.getenv("GIT_REPO_LOC"), "Users/cusgadmin/Documents/2014fall/STAT215/Lab/Lab4_repo/Lab4"))
source('elcm_qda_classifier.R')
source('multiplot.R')
source('crf_classifier.R')


# Get the data for three images

# New classifier code:
image.one <- read.table('image1.txt', header=F)
image.three <- read.table('image3.txt', header=F)
image.two <- read.table('image2.txt', header=F)
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image.one) <- collabs
names(image.two) <- collabs
names(image.three) <- collabs
params <- TrainELCMQDA(image.one, image.one$label)
image.one$label.pre <- CostELCMQDA(image.one, image.one$label, params)
p1<-ggplot(image.one) + geom_point(aes(x=x, y=y, color=factor(label.pre))) +
  ggtitle('Predicted labels for image1')
p2<-ggplot(image.one) + geom_point(aes(x=x, y=y, color=factor(label))) +
  ggtitle('True labels for image1')



params <- TrainELCMQDA(image.two, image.two$label)
image.two$label.pre <- CostELCMQDA(image.two, image.two$label, params)
p3<-ggplot(image.two) + geom_point(aes(x=x, y=y, color=factor(label.pre))) +
  ggtitle('Predicted labels for image2')
p4<-ggplot(image.two) + geom_point(aes(x=x, y=y, color=factor(label))) +
  ggtitle('True labels for image2')



params <- TrainELCMQDA(image.three, image.three$label)
image.three$label.pre <- CostELCMQDA(image.three, image.three$label, params)
p5<-ggplot(image.three) + geom_point(aes(x=x, y=y, color=factor(label.pre))) +
  ggtitle('Predicted labels for image3')
p6<-ggplot(image.three) + geom_point(aes(x=x, y=y, color=factor(label))) +
  ggtitle('True labels for image3')

multiplot(p1, p3, p5, p2, p4, p6, cols = 2)


## conditional random field


image.one$label.pre <- as.numeric(crf_classifier(image.one,5, 0.2))
p1 <- ggplot(image.one) + geom_point(aes(x=x, y=y, color=factor(label.pre))) +
  ggtitle('Predicted labels for image1')
p2<-ggplot(image.one) + geom_point(aes(x=x, y=y, color=factor(label))) +
  ggtitle('True labels for image1')


image.two$label.pre <- as.numeric(crf_classifier(image.two,5, 0.2))
p3<-ggplot(image.two) + geom_point(aes(x=x, y=y, color=factor(label.pre))) +
  ggtitle('Predicted labels for image2')
p4<-ggplot(image.two) + geom_point(aes(x=x, y=y, color=factor(label))) +
  ggtitle('True labels for image2')



image.three$label.pre <- as.numeric(crf_classifier(image.three,5, 0.2))
p5<-ggplot(image.three) + geom_point(aes(x=x, y=y, color=factor(label.pre))) +
  ggtitle('Predicted labels for image3')
p6<-ggplot(image.three) + geom_point(aes(x=x, y=y, color=factor(label))) +
  ggtitle('True labels for image3')

multiplot(p1, p3, p5, p2, p4, p6, cols = 2)
