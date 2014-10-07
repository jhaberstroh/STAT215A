options(warn=0)
setwd("~/Courses/STAT215/STAT215A-Lab2")
library(maps)
library(ggplot2)
library(dplyr)
library(reshape2)
library(RANN)
library(proxy)

source('jhaberstroh_ling_methods.R')

# ----------------------------------------
# Perform cleaning 
# ----------------------------------------
ling.data <- ling.data[complete.cases(ling.data), ]

# ----------------------------------------
# Create binary dataset 
# ----------------------------------------
ling.identify <- c('ID','CITY','STATE','ZIP','lat','long')
ling.subset <- c('CITY','STATE','ZIP','lat','long')
ling.ans <- names(ling.data)[!(names(ling.data) %in% ling.identify)]
ling.data.bin <- BinarizeAnswers(ling.data[, !names(ling.data) %in% ling.subset],'ID')
head(ling.data.bin)
ling.data.bin <- inner_join(ling.data[,ling.identify], ling.data.bin, by='ID')
remove(ling.identify)
remove(ling.subset)
remove(ling.ans)

# ----------------------------------------
# Create training/validation dataset in binary
# ----------------------------------------
n.train <- 10000
n.rows <- nrow(ling.data.bin)
ling.samples.train <- sample(n.rows,n.train)
ling.data.bin.train <- ling.data.bin[ling.samples.train,]
ling.data.bin.valid <- ling.data.bin[!seq(1, n.rows) %in% ling.samples.train,]

# ----------------------------------------
# Perform heirarchical clustering on all of the data 
# ----------------------------------------
SeguyDist <- function(data) dist(data, method=function(x, y)  sum(x!=y))

ling.ans <- names(ling.data)[!names(ling.data) %in% c('ID', 'CITY','STATE','ZIP','lat','long')]
n.sample <- 4000
ling.data.mainland <- filter(ling.data, -65 > long & long > -125 & 50 > lat & lat > 25)
ling.data.hclust <- ling.data.mainland[sample(nrow(ling.data.mainland),n.sample), ]
ling.data.hclust <- ApplyHClust(ling.data.hclust, 
                                dist_method=SeguyDist, 
                                cluster.cols=ling.ans)
ggplot(ling.data.hclust, aes(x=long, y=lat, color=cluster8)) +
  geom_point(alpha=.8) +
  scale_color_brewer(palette = 'Set1')




# ----------------------------------------
# Perform a PCA on the binary data without spatial distribution
# ----------------------------------------
#ling.data.bin.centered <- ling.data.bin - colMeans(ling.data.bin)
ling.data.bin.pca <- prcomp(ling.data.bin.train[,!names(ling.data.bin.train) %in% ling.identify])
qplot(seq_along(ling.data.bin.pca$sdev), ling.data.bin.pca$sdev)
plot(ling.data.bin.pca$sdev)


# ----------------------------------------
# Plot lat & long of ling.data.bin
# Plot maximally redundant lat/long pairs
# ----------------------------------------
ggplot(data=NULL) +
  plot.map + 
  plot.blank +
  geom_point(data=ling.data.bin, aes(x=long, y=lat, color=Q050.1), size=3, alpha=0.5)
ggplot(data=NULL) +
  plot.map + 
  plot.blank +
  geom_point(data=ling.data.bin.mainland, aes(x=long, y=lat, color=Q050.1), size=3, alpha=0.5)
ling.data.bin.mainland <- filter(ling.data.bin, lat < 53 & long > -140)
ling.data.bin.mainland.latlongagg <- group_by(ling.data.bin.mainland, lat, long) %>% 
  summarize(n=n())
max = head(ling.data.bin.mainland.latlongagg[
    with(ling.data.bin.mainland.latlongagg, order(-n)),],100)
ggplot(data=NULL) +
  plot.map + 
  plot.blank +
  geom_point(data=max, aes(x=long, y=lat, color=n), size=3, alpha=.9)
remove(ling.data.bin.mainland.latlongagg)


# ----------------------------------------
# Clustering with nn
# ----------------------------------------

# # Create nearest neighbor spatial gridding
# # ----------------------------------------
# ling.identify <- c('ID','CITY','STATE','ZIP','lat','long')
# ling.ans <- names(ling.data.bin)[!names(ling.data.bin) %in% ling.identify]
# spatial.nn.bin <- GetNNSum(ling.data.bin, 
#                            CreateGrid(seq(-125,-65,.5), seq(25,50,.5)), 
#                            cols.pos=c('lat','long'), cols.sum=ling.ans)
# # Perform hclust on nn grid
# # ----------------------------------------
# spatial.nn.bin.copy <- ApplyHClust(spatial.nn.bin, dist_method=dist,
#                                    cluster.cols=ling.ans)
# 
save(spatial.nn.bin, spatial.nn.bin.copy, file='hclust_file.Rda')
# Plot the nn spatial map for cluster
# ----------------------------------------
load('hclust_file.Rda')
ggplot(spatial.nn.bin.copy) +
  geom_point(aes(x=long,y=lat,color=cluster6), shape=15, size = 4, alpha=.6) +
  plot.map +
  plot.blank +
  scale_color_brewer(palette = 'Set3')
# Plot the nn spatial map for column Q050.4 (arbitrary)
# ----------------------------------------
ggplot(spatial.nn.bin) +
  geom_point(aes(x=long,y=lat,color=Q050.4), shape=7) +
  plot.map +
  plot.blank


# ----------------------------------------
# Principal components on grid data
# ----------------------------------------
#ling.data.bin.centered <- ling.data.bin - colMeans(ling.data.bin)
nrow(filter(spatial.nn.bin.copy,cluster2==1))
nn.pca.all <- CenteredPCA(filter(spatial.nn.bin.copy),
                          ling.ans)
nn.pca.c6.3 <- CenteredPCA(filter(spatial.nn.bin.copy, cluster6 == 3)
                           ,ling.ans)

PCAComponentPlot(nn.pca.all$rot, 20,end=TRUE)



plot(ling.data.bin.pca$sdev)


