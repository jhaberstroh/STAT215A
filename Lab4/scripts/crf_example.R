#------------------ BEGIN HEADER -----------------------
#------------------ BEGIN HEADER -----------------------
#------------------ BEGIN HEADER -----------------------

library(ggplot2)
library(dplyr)
library(optparse)
library(yaml)
library(lattice)

# Set true if running interactively to allow specification of args$config by hand.
interactive = FALSE 

# Read the command arguments to find the configuration file.
option_list <- list(make_option(c("-c","--config"), default="cfg.yml", help="Location of .yml configuration file"))
parser <- OptionParser(usage = "Hello", option_list = option_list)
args = parse_args(parser)
if (interactive) {
  args$config <- 'config.yml'  
}

# Load the yaml configuration file.
yaml <- yaml.load_file(args$config)
print(paste("Project directory:",yaml$config$dir))

# Source our helper functions for this script.
setwd(paste0(yaml$config$dir,'/R/'))
source(paste0(yaml$config$dir,'/R/','elcm_qda_classifier.R'))
source(paste0(yaml$config$dir,'/R/','crf_classifier.R'))
source(paste0(yaml$config$dir,'/R/','multiplot.R'))

# Create a lambda to stitch together the path and extension matching 
# this script's configuration with only the 'filename' specified.
figname <- function(filename){
  return (paste0(yaml$config$dir,'/figures/',filename,'.png'))
}

#------------------  END  HEADER -----------------------
#------------------  END  HEADER -----------------------
#------------------  END  HEADER -----------------------

# Get the data for three images

# New classifier code:
image.one = read.table(paste0(yaml$config$data, "/image1.txt"), header=F)
image.two = read.table(paste0(yaml$config$data, "/image2.txt"), header=F)
image.three = read.table(paste0(yaml$config$data, "/image3.txt"), header=F)


collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image.one) <- collabs
names(image.two) <- collabs
names(image.three) <- collabs

# Generate result from ELCMQDA for comparison

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

png(figname('ELCMQDA'), width=15, height=12,units="in", res=300)
# trellis.par.set("superpose.line",png_pars.lines12) # set line colors & types in png 
# trellis.par.set("superpose.symbol",png_pars.symbols12) # set symbol types & colors in png

multiplot(p1, p3, p5, p2, p4, p6, cols = 2)

dev.off() # turn off png device,



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

png(figname('CRF'), width=15, height=12,units="in", res=300)
# trellis.par.set("superpose.line",png_pars.lines12) # set line colors & types in png 
# trellis.par.set("superpose.symbol",png_pars.symbols12) # set symbol types & colors in png

multiplot(p1, p3, p5, p2, p4, p6, cols = 2)

dev.off() # turn off png device,


