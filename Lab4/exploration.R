library(ggplot2)
library(dplyr)

wd <- "/Users/newuser/Courses/STAT215/Lab4"
image.one = read.table(paste0(wd, "/", "data/image1.txt"), header=F)
image.two = read.table(paste0(wd, "/", "data/image2.txt"), header=F)
# Add informative column names.
column.labels <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image.one) <- column.labels
names(image.two) <- column.labels


source(paste0(wd,'/','exploration_func.R'))

head(image.one)
summary(image.one)

# The raw image (red band, from nadir).
ggplot(image.one) + geom_point(aes(x=x, y=y, color=AN))
ggplot(image.two) + geom_point(aes(x=x, y=y, color=AN))
# The classification.
ggplot(image.one) + geom_point(aes(x=x, y=y, color=factor(label)))
ggplot(image.two) + geom_point(aes(x=x, y=y, color=factor(label)))
# Class conditional densities.
ggplot(image.one) + geom_density(aes(x=AN, group=factor(label), fill=factor(label)), alpha=0.5)

ggplot(image.one) + geom_density(aes(x=NDAI, group=factor(label), fill=factor(label)), alpha=0.5)
ggplot(image.one) + geom_density(aes(x=SD, group=factor(label), fill=factor(label)), alpha=0.5)
ggplot(image.one) + geom_density(aes(x=CORR, group=factor(label), fill=factor(label)), alpha=0.5)
ggplot(image.one) + 
  geom_point(aes(x=SD, y=CORR, color=factor(label)), alpha=.5, size=.5)
ggplot(image.one) + 
  geom_point(aes(x=NDAI, y=CORR, color=factor(label)), alpha=.5, size=.5)


ggplot(image.one) + 
  geom_point(aes(x=NDAI, y=SD, color=factor(label)), alpha=.5, size=.5)

ggplot(image.one.ndai) +
  geom_point(aes(x=NDAI, y=SD), alpha=.5, size=.5)
ggplot(image.one.ndai) +
  geom_point(aes(x=CORR, y=SD), alpha=.5, size=.5)

ggplot(image.one) + 
  geom_point(aes(x=AN, y=CORR, color=factor(label)), alpha=.5, size=.5) + 
  geom_point(data = image.one.ndai, aes(x=AN, y=CORR), alpha=.5, size=1)
ggplot(image.one) + 
  geom_point(aes(x=CF, y=DF, color=factor(label)), alpha=.5, size=.5) + 
  geom_point(data = image.one.ndai, aes(x=CF, y=DF), alpha=.5, size=1)
x.string <- 'CORR'
y.string <- 'DF'

source(paste0(wd,'/','exploration_func.R'))
image.one.sample <- image.one[sample.int(nrow(image.one), 10000, replace=FALSE),]
# Classify using cutoff on NDAI & SD first, leaving behind misclassified entries
image.one.mc1 <- filter(image.one, NDAI > 0 & (NDAI > 1.5| SD > 5) & label==-1)
SingleScatter(image.one.sample, 'CORR', 'SD')
DoubleDensity(image.one.sample, image.one.mc1, 'AN')
# Classify using AN second, leaving behind misclassified entries
image.one.mc2 <- filter(image.one.mc1, AN < 200)
# Remaining classifications are difficult to partition
DoubleDensity(image.one.sample, image.one.mc2, 'AN')
DoubleScatter(image.one.sample, image.one.mc2, 'NDAI', 'SD')
DoubleDensity(image.one.sample, image.one.mc2, 'AF')

# -------------- EXPLORATION 2 -----------------
# Begin with a high/low AN partition
SingleDensity(image.one.sample, 'AN')
SingleScatter(image.one.sample, 'NDAI', 'SD')
image.one.AN.low <- filter(image.one, AN < 200)
SingleScatter(image.one.AN.low, 'NDAI', 'SD')
image.one.AN.high <- filter(image.one, AN > 200)
SingleScatter(image.one.AN.high, 'NDAI', 'SD')
group_by(image.one.AN.low, label) %>% summarise(n())
group_by(image.one.AN.high, label) %>% summarise(n())
# Notice the high AN partition is the same region of clusering in NDAI & SD
SingleScatter(filter(image.one.AN.high, label == -1), 'NDAI', 'SD')
SingleScatter(filter(image.one.AN.high, label != 0), 'NDAI', 'SD')
SingleScatter(filter(image.one.sample, label != 0), 'NDAI', 'SD')
# Notice the low AN partition is missing that clustering region
SingleScatter(filter(image.one.AN.low, label != 0), 'NDAI', 'SD')
# TAKE HOME: Regress on high and low AN separately

# Notice that none of these low AN plots show strong differences
SingleDensity(filter(image.one.AN.low, label != 0), 'AN')
SingleDensity(filter(image.one.AN.low, label != 0), 'AF')
SingleDensity(filter(image.one.AN.low, label != 0), 'BF')
SingleDensity(filter(image.one.AN.low, label != 0), 'CF')
SingleDensity(filter(image.one.AN.low, label != 0), 'DF')
SingleDensity(filter(image.one.AN.low, label != 0), 'SD') #SD > 30 is a good partition
SingleDensity(filter(image.one.AN.low, label != 0), 'CORR') # CORR < 0 is a good partition
SingleDensity(filter(image.one.AN.low, label != 0), 'NDAI') # High NDAI is an OK partition
# Look into joints
SingleScatter(filter(image.one.AN.low, label != 0), 'AF', 'DF')
SingleScatter(filter(image.one.AN.low, label != 0), 'BF', 'DF')
SingleScatter(filter(image.one.AN.low, label != 0), 'CF', 'DF')
SingleScatter(filter(image.one.AN.low, label != 0), 'NDAI', 'DF')
SingleScatter(filter(image.one.AN.low, label != 0), 'NDAI', 'CORR')
# TAKE HOME: Should use neighbor data and gradients 





