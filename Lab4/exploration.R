library(ggplot2)
library(dplyr)
library(optparse)
library(yaml)
# Set true if running interactively to allow specification of args$config by hand.
interactive = FALSE 

# Read the command arguments to find the configuration file.
option_list <- list(make_option(c("-c","--config"), default="cfg.yml", help="Location of .yml configuration file"))
parser <- OptionParser(usage = "Hello", option_list = option_list)
args = parse_args(parser)
print(args$config)
if (interactive) {
  args$config <- 'config.yml'  
}

# Load the yaml configuration file.
yaml <- yaml.load_file(args$config)
print(yaml$config$dir)
print(yaml$config$figure)

# Source our helper functions for this script.
source(paste0(yaml$config$dir,'/R/exploration_func.R'))

# Create a lambda to stitch together the path and extension matching 
# this script's configuration with only the 'filename' specified.
figname <- function(filename){
  return (paste0(yaml$config$figure,'/',filename,'.png'))
}

# Read the datasets image1.txt and image2.txt.
image.one = read.table(paste0(yaml$config$data, "/image1.txt"), header=F)
image.two = read.table(paste0(yaml$config$data, "/image2.txt"), header=F)
# Add informative column names.
column.labels <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image.one) <- column.labels
names(image.two) <- column.labels
image.one.sample <- image.one[sample.int(nrow(image.one), 10000, replace=FALSE),]


# -------------- EXPLORATION 1 -----------------
# The raw image (red band, from nadir).
ggplot(image.one) + geom_point(aes(x=x, y=y, color=AN))
ggplot(image.two) + geom_point(aes(x=x, y=y, color=AN))
# The classification.
ggplot(image.one) + geom_point(aes(x=x, y=y, color=factor(label)))
ggplot(image.two) + geom_point(aes(x=x, y=y, color=factor(label)))
# Class conditional densities.
ggplot(image.one) + geom_density(aes(x=AN, group=factor(label), fill=factor(label)), alpha=0.5)



# -------------- EXPLORATION 2 -----------------
source(paste0(yaml$config$dir,'/R/exploration_func.R'))
size <- c(6, 4)
# Begin with a high/low AN partition
SingleDensity(image.one.sample, 'AN', 
              title= 'Density plot of AN from random sample',
              save = figname('density-AN'),
              size = size)
SingleDensity(image.one.sample, 'CORR', 
              title= 'Density plot of CORR from random sample',
              save = figname('density-CORR'),
              size = size)
SingleDensity(image.one.sample, 'SD', 
              title= 'Density plot of SD from random sample',
              save = figname('density-SD'),
              size = size)
SingleDensity(image.one.sample, 'NDAI', 
              title= 'Density plot of NDAI from random sample',
              save = figname('density-NDAI'),
              size = size)




SingleScatter(image.one.sample, 'NDAI', 'SD',
              title = 'Scatter of all NDAI vs SD from random sample',
              save = figname('scatter_NDAI-SD'),
              size = size)
# Demonstrate low/high AN as a good partition
image.one.AN.low <- filter(image.one, AN < 200)
SingleScatter(image.one.AN.low, 'NDAI', 'SD',
              title = 'Scatter of NDAI vs SD from low AN (AN < 200)',
              save  = figname('scatterLowAN_NDAI-SD'),
              size = size)
image.one.AN.high <- filter(image.one, AN > 200)
SingleScatter(image.one.AN.high, 'NDAI', 'SD',
              title = 'Scatter of NDAI vs SD from high AN (AN > 200)',
              save  = figname('scatterHighAN_NDAI-SD'),
              size = size)
SingleScatter(filter(image.one.AN.low, label != 0), 'NDAI', 'CORR',
              title= 'Joint density of low AN',
              save = figname('scatterLowAN_NDAI-CORR'),
              size = size)

group_by(image.one.AN.low, label) %>% summarise(n())
group_by(image.one.AN.high, label) %>% summarise(n())
# # Notice the high AN partition is the same region of clusering in NDAI & SD
# SingleScatter(filter(image.one.AN.high, label == -1), 'NDAI', 'SD')
# SingleScatter(filter(image.one.AN.high, label != 0), 'NDAI', 'SD')
# SingleScatter(filter(image.one.sample, label != 0), 'NDAI', 'SD')
# # Notice the low AN partition is missing that clustering region
# SingleScatter(filter(image.one.AN.low, label != 0), 'NDAI', 'SD')
# # TAKE HOME: Regress on high and low AN separately

# Notice that none of these low AN plots show strong differences
SingleDensity(filter(image.one.AN.low, label != 0), 'AN')
SingleDensity(filter(image.one.AN.low, label != 0), 'AF',
              title= 'AF density from low AN',
              save = figname('densityLowAN-AF'),
              size = size)
SingleDensity(filter(image.one.AN.low, label != 0), 'BF',
              title= 'BF density from low AN',
              save = figname('densityLowAN-BF'),
              size = size)
SingleDensity(filter(image.one.AN.low, label != 0), 'CF',
              title= 'CF density from low AN',
              save = figname('densityLowAN-CF'),
              size = size)
SingleDensity(filter(image.one.AN.low, label != 0), 'DF',
              title= 'DF density from low AN',
              save = figname('densityLowAN-DF'),
              size = size)
SingleDensity(filter(image.one.AN.low, label != 0), 'SD') #SD > 30 is a good partition
SingleDensity(filter(image.one.AN.low, label != 0), 'CORR') # CORR < 0 is a good partition
SingleDensity(filter(image.one.AN.low, label != 0), 'NDAI') # High NDAI is an OK partition

              
# TAKE HOME: Should use neighbor data and gradients 





