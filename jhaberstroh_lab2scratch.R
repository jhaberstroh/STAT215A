options(warn=0)
setwd("~/Courses/STAT215/STAT215A-Lab2")
library(maps)
library(ggplot2)
library(dplyr)
library(reshape2)
library(RANN)
library(proxy)

load('ling.RData')
load("question_data.RData")
state.df <- map_data("state")

plot.blank <-
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 
plot.map <-
  geom_polygon(data=state.df, colour = "black", 
               fill = NA, aes(x=long, y=lat, group=group))


BinarizeAnswers <- function(data, cols.identify){
  data.melt <- melt(data, id.vars=cols.identify)
  data.melt$answer <- paste(data.melt$variable,'.',data.melt$value, sep='')
  data.melt <- filter(data.melt, value != 0)
  data.melt <- data.melt[,!(names(data.melt) %in% c('variable','value'))]
  data.melt$bin <- 1
  #return(data.melt)
  data.cast <- dcast(data.melt, ID ~ answer, value.var='bin', fill=0)
  return(data.cast)
}

# ----------------------------------------
# Perform cleaning 
# ----------------------------------------
ling.data <- filter(ling.data, !is.na(lat) & !is.na(long))

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
# Perform heirarchical clustering with a simple 
# ----------------------------------------
SeguyDist <- function(x, y)  sum(x!=y)

ling.data.subset <- !names(ling.data) %in% c('ID', 'CITY','STATE','ZIP','lat','long')
#ling.data.filter <- filter(ling.data, !is.na(lat) & !is.na(long))
ling.data.filter <- ling.data[complete.cases(ling.data), ]
n.sample <- 100
ling.data.sample <- ling.data.filter[sample(nrow(ling.data.filter),n.sample), ling.data.subset]
d <- dist(ling.data.sample, method=SeguyDist)
clust.hierarchical <- hclust(d)
plot(clust.hierarchical, cex=.4)



# ----------------------------------------
# Perform a PCA on the binary data without spatial distribution
# ----------------------------------------
#ling.data.bin.centered <- ling.data.bin - colMeans(ling.data.bin)
ling.data.bin.pca <- prcomp(ling.data.bin.train[,!names(ling.data.bin.train) %in% 'ID'])
ggplot(ling.data.bin.pca$sdev) +
  geom_line()
plot(ling.data.bin.pca$sdev)




# ----------------------------------------
# Plot lat & long of ling.data.bin
# Plot maximally redundant lat/long pairs
# ----------------------------------------
ggplot(data=NULL) +
  plot.map + 
  plot.blank +
  geom_point(data=ling.data.bin, aes(x=long, y=lat, color=Q050.1), size=3, alpha=0.5)
ling.data.bin.mainland <- filter(ling.data.bin, lat < 53 & long > -140)
ggplot(data=NULL) +
  plot.map + 
  plot.blank +
  geom_point(data=ling.data.bin.mainland, aes(x=long, y=lat, color=Q050.1), size=3, alpha=0.5)
ling.data.bin.mainland.latlongagg <- group_by(ling.data.bin.mainland, lat, long) %>% 
  summarize(n=n())
max = head(ling.data.bin.mainland.latlongagg[
    with(ling.data.bin.mainland.latlongagg, order(-n)),],20)
ggplot(data=NULL) +
  plot.map + 
  plot.blank +
  geom_point(data=max, aes(x=long, y=lat), size=3, alpha=0.5)
remove(ling.data.bin.mainland.latlongagg)


# ----------------------------------------
# Create nearest neighbor spatial gridding
# ----------------------------------------
spatial = data.frame(long=seq(-125,-70, .5))
spatial$all = 1
spatial.lat = data.frame(lat=seq(25,50,.5))
spatial.lat$all = 1
spatial <- inner_join(spatial, spatial.lat, by='all')
spatial <- spatial[,!names(spatial) %in% c('all')]
remove(spatial.lat)
spatial.bin.merger <- data.frame(matrix(vector(), nrow(spatial), length(ling.bin.ans), 
                                        dimnames=list(c(), ling.bin.ans)), 
                                 stringsAsFactors=F)
spatial.nn.bin <- cbind(spatial[,c('long','lat')], spatial.bin.merger)
remove(spatial.bin.merger)

ling.identify <- c('ID','CITY','STATE','ZIP','lat','long')
ling.bin.ans <- names(ling.data.bin)[!names(ling.data.bin) %in% ling.identify]
spatial.nnlist <- nn2(ling.data.bin[,c('long','lat')], 
                      spatial.bin[,c('long','lat')], k=100)
for (i in 1:nrow(spatial)) {
  spatial.nn.bin[i,ling.bin.ans] <- 
    colSums(sapply(
      ling.data.bin[spatial.nnlist$nn.idx[i,],ling.bin.ans],as.numeric))
}
remove(spatial.nnlist)
ggplot(spatial.nn.bin) +
  geom_point(aes(x=long,y=lat,color=Q050.4), shape=7) +
  plot.map +
  plot.blank





all.ans[['50']]

plural.second.person <- filter(ling.data, Q050 %in% c(1, 4, 7, 9), long > -125)
answers.q50 <- all.ans[['50']]
answers.q50$Q050 <- rownames(answers.q50)

plural.second.person$Q050 <- as.character(plural.second.person$Q050)
plural.second.person <- inner_join(plural.second.person, answers.q50, by="Q050")



spatial <- spatial[,c('long','lat')]
spatial$index <- as.numeric(row.names(spatial))

nnlist <- nn2(ling.data.clean[,c('long','lat')], spatial,k=100)
# spatial.nn.idx <- data.frame(nnlist$nn.idx)
# spatial.nn.idx$index <- row.names(spatial.nn.idx)
# spatial <- inner_join(spatial, spatial.nn.idx, by='index')

# spatial.cols = names(spatial.nn.idx)
# spatial.cols <- spatial.cols[1:20]
# spatial.nn.idx <- spatial.nn.idx[,spatial.cols]




ling.ans.names
spatial.ans.density

for (col in ling.ans.names) {
  a <- rep(NA, max(spatial$index))
  for (row in spatial$index) {
    x<- ling.data[nnlist$nn.idx[row,],ling.ans.names]
    y<- group_by(x,'Q050') %>% summarise(n=n()/100)
    a[row] <- max(y$n)
  }
}


for (row in spatial$index){
}

all.ans.dframe <- data.frame(all.ans)
names(all.ans)
all.ans.dframe <- melt.list(all.ans)


ggplot(spatial) +
  geom_point(aes(x=long,y=lat))

ggplot(data=NULL) +
  plot.map + 
  plot.blank +
  geom_point(data=plural.second.person, aes(x=long, y=lat, color=ans), size=3, alpha=0.5)
