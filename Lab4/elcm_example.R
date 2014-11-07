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

image.one <- read.table('data/image1.txt', header=F)
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image.one) <- collabs

search.NDAI <- seq(0,2,0.001)
ndai.min <- FindMinNDAI(image.one, search.NDAI, .075, 2)
ndai.min
