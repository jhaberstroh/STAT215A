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
image.one.sample <- image.one[sample.int(nrow(image.one), 1000, replace=FALSE),]
image.one.AN <- filter(image.one, AN > 200)
image.one.AN.sample <- 
  image.one.AN[sample.int(nrow(image.one.AN), 4000, replace=FALSE),]



# -------------- Scatterplot matrices -----------------
# Create the full scatterplot matrix
png(figname('splom'), width=15, height=12,units="in", res=300)
trellis.par.set("superpose.line",png_pars.lines12) # set line colors & types in png 
trellis.par.set("superpose.symbol",png_pars.symbols12) # set symbol types & colors in png
super.sym <- trellis.par.get("superpose.symbol")
splom(~image.one.sample[4:11], 
      data = filter(image.one.sample, label != 0),
      groups=label,
      panel = panel.superpose,
      key = list(title = "Scatter plot matrix of a random sample of all data",
                 columns = 2,
                  points = list(pch = super.sym$pch[1:2],
                                col = super.sym$col[1:2]),
                 text = list(c("Cloud", "Snow"))),
      alpha=.8)
dev.off() # turn off png device,

# Create the filtered scatterplot matrix
png(figname('splom_ANhigh'), width=15, height=12,units="in", res=300)
trellis.par.set("superpose.line",png_pars.lines12) # set line colors & types in png 
trellis.par.set("superpose.symbol",png_pars.symbols12) # set symbol types & colors in png
super.sym <- trellis.par.get("superpose.symbol")
splom(~image.one.sample[4:6], 
      data = filter(image.one.AN.sample, label != 0),
      groups=label,
      panel = panel.superpose,
      key = list(title = "Scatter plot matrix for all AN > 200",
                 columns = 2,
                 points = list(pch = super.sym$pch[1:2],
                               col = super.sym$col[1:2]),
                 text = list(c("Cloud", "Snow"))),
      alpha=.8)
dev.off() # turn off png device,


# -------------- 1-D density plots -----------------
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




# -------------- Classifications -----------------
# The classification.
ggplot(image.one) + geom_point(aes(x=x, y=y, color=factor(label))) +
  ggsave(filename= figname('labels1'))
ggplot(image.two) + geom_point(aes(x=x, y=y, color=factor(label))) + 
  ggsave(filename= figname('labels2'))






