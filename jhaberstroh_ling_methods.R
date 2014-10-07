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

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


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

CreateGrid <- function(xseq, yseq){
  spatial = data.frame(long=xseq)
  spatial$all = 1
  spatial.lat = data.frame(lat=yseq)
  spatial.lat$all = 1
  spatial <- inner_join(spatial, spatial.lat, by='all')
  spatial <- spatial[,!names(spatial) %in% c('all')]
  remove(spatial.lat)
  return(spatial)
}

GetNNSum <- function(dataset, spatial, cols.pos, cols.sum, k=100){
  # Create an empty dataset to populate
  spatial.bin.merger <- data.frame(matrix(vector(), nrow(spatial), 
                                          length(cols.sum), dimnames=list(c(), cols.sum)), 
                                   stringsAsFactors=F)
  spatial.nn.bin <- cbind(spatial[,c('long','lat')], spatial.bin.merger)
  remove(spatial.bin.merger)
  
  #ling.bin.ans <- names(dataset)[cols.sum]
  spatial.nnlist <- nn2(dataset[,cols.pos], 
                        spatial.nn.bin[,cols.pos], k=k)
  for (i in 1:nrow(spatial)) {
    spatial.nn.bin[i,cols.sum] <- 
      colSums(sapply(
        ling.data.bin[spatial.nnlist$nn.idx[i,],cols.sum],as.numeric))
  }
  return(spatial.nn.bin)
}

ApplyHClust <- function(dataset, dist_method, cluster.cols) {
  clusters <- hclust(dist_method(dataset[,cluster.cols]))
  clusters.tree <- cutree(clusters, k=2)
  dataset[, 'cluster2'] <- sapply(clusters.tree, as.character)
  clusters.tree <- cutree(clusters, k=3)
  dataset[, 'cluster3'] <- sapply(clusters.tree, as.character)
  clusters.tree <- cutree(clusters, k=4)
  dataset[, 'cluster4'] <- sapply(clusters.tree, as.character)
  clusters.tree <- cutree(clusters, k=5)
  dataset[, 'cluster5'] <- sapply(clusters.tree, as.character)
  clusters.tree <- cutree(clusters, k=6)
  dataset[, 'cluster6'] <- sapply(clusters.tree, as.character)
  clusters.tree <- cutree(clusters, k=7)
  dataset[, 'cluster7'] <- sapply(clusters.tree, as.character)
  clusters.tree <- cutree(clusters, k=8)
  dataset[, 'cluster8'] <- sapply(clusters.tree, as.character)
  return(dataset)
}


CenteredPCA <- function(data, cols){
  center <- data[,cols] - colMeans(data[,cols])
  return(prcomp(center))
}
PCAComponentPlot <- function(data, ncols,end=FALSE, title=""){
  if (end){
    squared = data[,(ncol(data)-ncols):ncol(data)] ** 2
  }
  else{
    squared = data[,1:ncols] ** 2
  }
  squared <- squared * (rowSums(squared) / rowSums(squared**.5) ** 2)
  squared <- squared ** .5
  squared.dframe <- data.frame(squared)
  squared.dframe$ID <- row.names(squared.dframe)
  squared.dframe <- melt(squared.dframe, eigenvec = 
                           names(squared.dframe)[names(squared.dframe) != 'ID'],
                         variable.name='eigenvec')
  return(ggplot(squared.dframe,aes(x=ID,y=value,fill=eigenvec)) + 
    geom_bar(stat='identity') +
    ggtitle(title))
}