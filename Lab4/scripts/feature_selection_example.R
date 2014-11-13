#------------------ BEGIN HEADER -----------------------
#------------------ BEGIN HEADER -----------------------
#------------------ BEGIN HEADER -----------------------

library(ggplot2)
library(dplyr)
library(optparse)
library(yaml)
library(lattice)
library(MASS)
library(FNN)
library(flexmix)
library(reshape2)


# Set true if running interactively to allow specification of args$config by hand.
interactive = FALSE 

# Read the command arguments to find the configuration file.
option_list <- list(make_option(c("-c","--config"), default="cfg.yml", help="Location of .yml configuration file"))
parser <- OptionParser(usage = "Hello", option_list = option_list)
args = parse_args(parser)
if (interactive) {
  args$config <- '/this/directory/config.yml'  
}

# Load the yaml configuration file.
yaml <- yaml.load_file(args$config)
print(paste("Project directory:",yaml$config$dir))

# Source our helper functions for this script.
source(paste0(yaml$config$dir,'/R/multiplot.R'))

# Create a lambda to stitch together the path and extension matching 
# this script's configuration with only the 'filename' specified.
figname <- function(filename){
  return (paste0(yaml$config$dir,'/figures/',filename,'.png'))
}

#------------------  END  HEADER -----------------------
#------------------  END  HEADER -----------------------
#------------------  END  HEADER -----------------------


# Get the data for three images
image.one = read.table(paste0(yaml$config$data, "/image1.txt"), header=F)
image.two = read.table(paste0(yaml$config$data, "/image2.txt"), header=F)
image.three = read.table(paste0(yaml$config$data, "/image3.txt"), header=F)

collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image.one) <- collabs
names(image.two) <- collabs
names(image.three) <- collabs


## Permutation test for feautre selection
# corresponding null hypothesis is "JS divergence of observed distributions 
# equals t"o JS divergence of distributions after randomly permuting the labels"
image <- rbind(image.one, image.two, image.three)

one.test <- function(label,feature) {
  label.perm <- sample(label)
  feature.cloud <- feature[label.perm == 1]
  feature.nocloud <- feature[label.perm == -1]
  min.length <- min(length(feature.cloud), length(feature.nocloud))
  ind <- sample(1:min.length,10000)
  JSD.perm <- KLdiv(cbind(feature.cloud[ind], feature.nocloud[ind]))
  return(0.5*sum(JSD.perm[1,2]+JSD.perm[2,1]))
}

# Permutation test of all features for 100 times
round <- 500
feature.name <- c('NDAI','SD','CORR','DF','CF','BF','AF','AN')
image.cloud <- subset(image, label == 1)
image.nocloud <- subset(image, label == -1)
JSD.perm <- matrix(rep(0,length(feature.name)*round), ncol = length(feature.name))
JSD.true <- rep(0, length(feature.name))
for (i in 1:length(feature.name)) {
  params <- feature.name[i]
  JSD.perm[,i] <- replicate(round, one.test(image$label,image[,params]))
  ind <- sample(1:min(length(image.cloud[,params]), 
                      length(image.nocloud[,params])),10000)
  JSD.temp <- KLdiv(cbind(image.cloud[,params][ind],image.nocloud[,params][ind]))
  JSD.true[i] <- 0.5*sum(JSD.temp[1,2]+JSD.temp[2,1])
}

JSD <- as.data.frame(JSD.perm)
names(JSD) <- feature.name


p1 <- ggplot(JSD) + geom_histogram(aes(x = NDAI), colour = 'blue', fill = 'blue', alpha = 0.5) + 
  geom_vline(xintercept = JSD.true[1], colour = 'red',  linetype = "longdash")
p2 <- ggplot(JSD) + geom_histogram(aes(x = SD), colour = 'blue', fill = 'blue', alpha = 0.5) + 
  geom_vline(xintercept = JSD.true[2], colour = 'red',  linetype = "longdash")
p3 <- ggplot(JSD) + geom_histogram(aes(x = CORR), colour = 'blue', fill = 'blue', alpha = 0.5) + 
  geom_vline(xintercept = JSD.true[3], colour = 'red',  linetype = "longdash")
p4 <- ggplot(JSD) + geom_histogram(aes(x = DF), colour = 'blue', fill = 'blue', alpha = 0.5) + 
  geom_vline(xintercept = JSD.true[4], colour = 'red',  linetype = "longdash")
p5 <- ggplot(JSD) + geom_histogram(aes(x = CF), colour = 'blue', fill = 'blue', alpha = 0.5) + 
  geom_vline(xintercept = JSD.true[5], colour = 'red',  linetype = "longdash")
p6 <- ggplot(JSD) + geom_histogram(aes(x = BF), colour = 'blue', fill = 'blue', alpha = 0.5) + 
  geom_vline(xintercept = JSD.true[6], colour = 'red',  linetype = "longdash")
p7 <- ggplot(JSD) + geom_histogram(aes(x = AF), colour = 'blue', fill = 'blue', alpha = 0.5) + 
  geom_vline(xintercept = JSD.true[7], colour = 'red',  linetype = "longdash")
p8 <- ggplot(JSD) + geom_histogram(aes(x = AN), colour = 'blue', fill = 'blue', alpha = 0.5) + 
  geom_vline(xintercept = JSD.true[8], colour = 'red',  linetype = "longdash")

png(figname('PermutationTest'), width=15, height=12,units="in", res=300)
# trellis.par.set("superpose.line",png_pars.lines12) # set line colors & types in png 
# trellis.par.set("superpose.symbol",png_pars.symbols12) # set symbol types & colors in png

multiplot(p1,p2,p3,p4,p5,p6,p7,p8,cols = 2)

dev.off() # turn off png device



## Stepwise selection based on AIC
image <- subset(rbind(image.one,image.two,image.three), label != 0)
fit <- glm(as.factor(label)~NDAI+SD+CORR+DF+CF+BF+AF+AN, 
           data=image,
           family=binomial(link="logit"))
step <- stepAIC(fit, direction = "both")
step$anova
# NDAI, SD, DF, CORR have highest AIC value




# further scrutiny using probability density plot
q1 <- ggplot(image) + geom_density(aes(x=NDAI, group=factor(label), fill=factor(label)), alpha=0.5)
q2 <- ggplot(image) + geom_density(aes(x=SD, group=factor(label), fill=factor(label)), alpha=0.5)
q3 <- ggplot(image) + geom_density(aes(x=CORR, group=factor(label), fill=factor(label)), alpha=0.5)
q4 <- ggplot(image) + geom_density(aes(x=DF, group=factor(label), fill=factor(label)), alpha=0.5)

png(figname('ConditionalDistribution'), width=15, height=12,units="in", res=300)
# trellis.par.set("superpose.line",png_pars.lines12) # set line colors & types in png 
# trellis.par.set("superpose.symbol",png_pars.symbols12) # set symbol types & colors in png

multiplot(q1,q2,q3,q4,cols = 1)
dev.off() # turn off png device



# PCA to visualize the seperability of different classes
image <- subset(image.one, label != 0)
sample.ind <- sample(1:nrow(image), 500)
image <- image[sample.ind,]

pca_all<-prcomp(image[,feature.name], scale = T)
screeplot(pca_all)
scores_all <- data.frame(image, pca_all$x[,1:3])
q1 <- qplot(x=PC1, y=PC2, data=scores_all, alpha = 0.8, color = factor(label), 
            shape=factor(label)) +
  theme(legend.position="none") + ggtitle('PCA with all features')

pca_good <-prcomp(image[,feature.name[1:4]], scale = T)
screeplot(pca_good)
scores_good <- data.frame(image, pca_good$x[,1:3])
q2 <- qplot(x=PC1, y=PC2, data=scores_good, alpha = 0.8, color = factor(label), 
            shape=factor(label)) +
  theme(legend.position="none") + ggtitle('PCA with NDAI,SD,CORR,DF')

pca_angle <- prcomp(image[,feature.name[5:8]], scale = T)
screeplot(pca_angle)
scores_angle <- data.frame(image, pca_angle$x[,1:3])
q3 <- qplot(x=PC1, y=PC2, data=scores_angle,  alpha = 0.8, color = factor(label), 
            shape=factor(label)) +
  theme(legend.position="none") + ggtitle('PCA with AN,AF,BF,CF')


png(figname('PCA_selection'), width=15, height=12,units="in", res=300)
# trellis.par.set("superpose.line",png_pars.lines12) # set line colors & types in png 
# trellis.par.set("superpose.symbol",png_pars.symbols12) # set symbol types & colors in png

multiplot(q1,q2,q3,cols = 1)
dev.off() # turn off png device


