library(optparse)

option_list <- list(make_option(c("-c","--config"), default="cfg.yml", help="Location of .yml configuration file"))
parser <- OptionParser(usage = "Hello", option_list = option_list)
args = parse_args(parser)
print(args$config)

library(dplyr)
library(ggplot2)
library(yaml)
yaml <- yaml.load_file(args$config)
print(yaml$config$dir)
print(yaml$config$dir)

source(paste0(yaml$config$dir,'/R/','elcm_qda_classifier.R'))
source(paste0(yaml$config$dir,'/R/','crf_classifier.R'))


#source('elcm_qda_classifier.R')
#source('ising.R')

#setwd(file.path("/Users/cusgadmin/Documents/2014fall/STAT215/Lab/Lab4/image_data"))
# Get the data for three images

# # New classifier code:
# image.one <- read.table('data/image1.txt', header=F)
# collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
# names(image.one) <- collabs
# params <- TrainELCMQDA(image.one, image.one$label)
# CostELCMQDA(image.one, image.one$label, params)
# 
# source('ising.R')
# class <- CostIsingPostCorrELCM(image.one, image.one$label, params, lambda=.3, iterations=20)
