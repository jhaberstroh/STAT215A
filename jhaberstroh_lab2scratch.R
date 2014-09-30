setwd("~/Courses/STAT215/Lab2")
options(warn=0)
library(maps)
library(ggplot2)
library(dplyr)
library(reshape2)
library(RANN)
library(proxy)
pr_DB$get_entry(2)

load('ling.RData')
load("question_data.RData")

names(ling.data)
names(ling.location)
state.df <- map_data("state")

ling.drops <- c('ID','CITY','STATE','ZIP','lat','long')
ling.ans.names <- names(ling.data)[!(names(ling.data) %in% ling.drops)]
ling.data <- filter(ling.data, !is.na(lat) & !is.na(long))
ling.sample <- ling.data[sample(nrow(ling.data),10), ling.ans.names]

surveydist <- function(x, y)  sum(x!=y)
ling.sample <- ling.data[sample(nrow(ling.data),1000), ling.ans.names]
colnames(ling.sample) <- (1:ncol(ling.sample))
quest.cols <- colnames(ling.sample)
ling.sample$id <- rownames(ling.sample)
ling.sample.melt <- melt(ling.sample, question = quest.cols)

d <- dist(ling.sample, method=surveydist)
clust <- hclust(d)

plot.blank <-
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 
plot.map <-
  geom_polygon(data=state.df, colour = "black", 
               fill = NA, aes(x=long, y=lat, group=group))

all.ans[['50']]

plural.second.person <- filter(ling.data, Q050 %in% c(1, 4, 7, 9), long > -125)
answers.q50 <- all.ans[['50']]
answers.q50$Q050 <- rownames(answers.q50)

plural.second.person$Q050 <- as.character(plural.second.person$Q050)
plural.second.person <- inner_join(plural.second.person, answers.q50, by="Q050")

spatial = data.frame(long=seq(-125,-70, .5))
spatial$all = 1
spatial.lat = data.frame(lat=seq(25,50,.5))
spatial.lat$all = 1
spatial <- inner_join(spatial, spatial.lat, by='all')
remove(spatial.lat)

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
