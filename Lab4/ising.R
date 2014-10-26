wd <- "/Users/newuser/Courses/STAT215/Lab4"
image.one = read.table(paste0(wd, "/", "data/image1.txt"), header=F)
# Add informative column names.
column.labels <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image.one) <- column.labels


library(Rcpp)
sourceCpp(paste0(wd, "/", "ising.cpp"))
image.one.extent <- c(min(image.one$x), max(image.one$x), min(image.one$y), max(image.one$y))

Jmtx <- LearnJ(image.one$x, image.one$y, image.one.extent, image.one$label)
