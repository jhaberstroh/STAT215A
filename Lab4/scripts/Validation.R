#------------------ BEGIN HEADER -----------------------
#------------------ BEGIN HEADER -----------------------
#------------------ BEGIN HEADER -----------------------

library(ggplot2)
library(dplyr)
library(optparse)
library(yaml)
library(lattice)
library(MASS)

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
setwd(paste0(yaml$config$dir,'/R'))
source(paste0(yaml$config$dir,'/R/ValidationLib.R'))
source(paste0(yaml$config$dir,'/R/elcm_qda_classifier.R'))
source(paste0(yaml$config$dir,'/R/crf_classifier.R'))
source(paste0(yaml$config$dir,'/R/crf_classifier_cv.R'))


# Create a lambda to stitch together the path and extension matching 
# this script's configuration with only the 'filename' specified.
figname <- function(filename){
  return (paste0(yaml$config$dir,'/figures/',filename,'.png'))
}

#------------------  END  HEADER -----------------------
#------------------  END  HEADER -----------------------
#------------------  END  HEADER -----------------------


# Get the data for three images
image1 = read.table(paste0(yaml$config$data,'/image1.txt'),header=F)
image2 = read.table(paste0(yaml$config$data,'/image2.txt'),header=F)
image3 = read.table(paste0(yaml$config$data,'/image3.txt'),header=F)

# Test
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image1) <- collabs
names(image2) <- collabs
names(image3) <- collabs

pred1 <- as.numeric(crf_classifier(image1,5, 0.2))
pred2 <- as.numeric(crf_classifier(image2,5, 0.2))
pred3 <- as.numeric(crf_classifier(image3,5, 0.2))

truth.label1 <- image1$label[image1$label!=0]
truth.label2 <- image2$label[image2$label!=0]
truth.label3 <- image3$label[image3$label!=0]

pred.label1 <- pred1[image1$label !=0]
pred.label2 <- pred2[image2$label !=0]
pred.label3 <- pred3[image3$label !=0]

truth.label1 <- truth.label1 >0
truth.label2 <- truth.label2 >0
truth.label3 <- truth.label3 >0

pred.label1 <- pred.label1 > 0
pred.label2 <- pred.label2 > 0
pred.label3 <- pred.label3 > 0

# Choose folds in cross validation
cl1 <- CVfold(image1, 5)
cl2 <- CVfold(image2, 5)
cl3 <- CVfold(image3, 5)

# Generate ROC curve & calculate AUC
print("Evaluating ROC for image 1. This will output about 15 statements over about 5 minutes. Please be patient ^.^")
png(figname('ROC_image1'), width=3, height=2.5,units="in", res=600)
label1 <- EvaluateModel(pred.label1, truth.label1)
dev.off()
print("Evaluating ROC for image 2. This will run silently for about 5 minutes. Please be patient ^.^")
png(figname('ROC_image2'), width=3, height=2.5,units="in", res=600)
label2 <- EvaluateModel(pred.label2, truth.label2)
dev.off()
print("Evaluating ROC for image 3. This will run silently for about 5 minutes. Please be patient ^.^")
png(figname('ROC_image3'), width=3, height=2.5,units="in", res=600)
label3 <- EvaluateModel(pred.label3, truth.label3)
dev.off()
