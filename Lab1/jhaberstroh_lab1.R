# In-class preliminary exploration of the redwood data for lab1.
library(ggplot2)
library(dplyr)
library(reshape2)

# Turn off my usual default of warnings as error.
options(warn=0)

# Load the data.
setwd('~/Courses/STAT215/lab1')
log <- read.csv('data/sonoma-data-log.csv',header=T)
net <- read.csv('data/sonoma-data-net.csv',header=T)
locs <- read.table('data/mote-location-data.txt')
save(log, file="data/log.Rda")
save(net, file="data/net.Rda")
save(locs, file="data/locs.Rda")
load("data/log.Rda")
load('data/net.Rda')
load('data/locs.Rda')
#all <- read.csv('data/sonoma-data-all.csv', header=T)

# Clean locs by converting to numerics -- WARNING: decimal data is mangled because language stinks
locs <- filter(locs, V1 !="ID") %>%  
  transform(V1 = as.numeric(V1), V2=as.numeric(V2)) %>% 
  arrange(V1,V2)
colnames(locs) <- c("nodeid", "height.m","direction","distance","tree")


# ================================================
# CLEAN DATA
# ================================================

# ====== log: convert time to dmin & date

# Give each row an identifier that specifies which dataset it came from
log$dset <- 'log'
# Convert time from POSIXlt to POSIXct
log$result_time <- as.POSIXlt( as.POSIXct(log$result_time) + 300 * log$epoch )
log$time.dmin <- log$result_time$hour * 60 + log$result_time$min
log$time.day <- log$result_time$yday
log$time.year <- log$result_time$year
log$time.isdst <- log$result_time$isdst
drops <- c("result_time")
log <- log[,!(names(log) %in% drops)]
remove(drops)
# Merge location data into log
log.merge <- inner_join(log, locs, by=c('nodeid'))
log.missing <- anti_join(log,locs, by=c('nodeid'))
log.missing <- log.missing[,c("nodeid","epoch")]


# ====== clean log (exploration)

ggplot(filter(log.merge, (time.dmin + time.day*1440) < 460000)) +
  geom_line(aes(x=(time.dmin + time.day*1440), y=humid_temp, group=nodeid))

unique( filter(log.merge, !(humid_temp > 0)) %>% select(nodeid) )
ggplot(filter(log.merge)) +
  geom_line(aes(x=(time.dmin + time.day*1440), y=humid_temp, group=nodeid, color=nodeid)) 

# Plot everything, line plot

# Check LATE DATA
ggplot(filter(log.merge,time.day >= 344)) +
  geom_line(aes(x=(time.dmin + time.day*1440)/1440, y=humid_temp, 
                group=nodeid, color=nodeid))

unique( filter(log.merge, time.day == 344) %>% select(nodeid) )


# Check HAMATOP 
ggplot(filter(log.merge,time.day < 320)) +
  geom_line(aes(x=(time.dmin + time.day*1440)/1440, y=hamatop,group=nodeid, color=nodeid)) + 
  geom_line(aes(x=(time.dmin + time.day*1440)/1440, y=voltage,group=nodeid, color=nodeid)) 

# HAMATOP with alpha 
ggplot(filter(log.merge, time.day > 340 & time.day <= 343)) +
  geom_line(aes(x=(time.dmin + time.day*1440)/1440, y=hamatop,
                group=nodeid, color=nodeid, alpha =.05))

# HAMATOP for a very cloudy day?
ggplot(filter(log.merge, time.day ==325 )) +
  geom_line(aes(x=(time.dmin + time.day*1440)/1440, y=hamatop,
                group=nodeid, color=nodeid, alpha =.05)) + 
  geom_line(aes(x=(time.dmin + time.day*1440)/1440, y=voltage,group=nodeid, color=nodeid)) 

# Near the end of the dataset
ggplot(filter(log.merge, time.day >= 343 & time.day < 346)) +
  geom_line(aes(x=(time.dmin + time.day*1440)/1440, y=hamatop,
                group=nodeid, color=nodeid, alpha =.05)) + 
  geom_line(aes(x=(time.dmin + time.day*1440)/1440, y=voltage,group=nodeid, color=nodeid)) 

# Check node 15
unique( filter(log.merge, is.na(humidity)) %>% select(nodeid) )
ggplot( filter(log.merge, nodeid == 15 | nodeid == 16)) +
  geom_line(aes(x=(time.dmin + time.day*1440)/1440, y=humid_temp,group=nodeid, color=nodeid)) +
  geom_point(aes(x=(time.dmin + time.day*1440)/1440, y=voltage,group=nodeid, color=nodeid)) 

ggplot( filter(log.merge, nodeid == 15, is.na(humid_temp)) ) +
  geom_point(aes(x=(time.dmin + time.day*1440), y=humid_temp)) +
  geom_point(aes(x=(time.dmin + time.day*1440), y=voltage)) 

# Check HAMABOT (is ok!)
ggplot(filter(log.merge, time.day > 310 & time.day < 330) ) +
  geom_point(aes(x=(time.dmin + time.day*1440)/1440, y=hamabot,
                 group=nodeid, color=nodeid, alpha=.1, size=.2)) + 
  geom_line(aes(x=(time.dmin + time.day*1440)/1440, y=hamatop,
                group=nodeid, color=nodeid, alpha=.05)) +
  scale_y_log10()

filter(log.merge.edge, hamabot > 7E3) %>% select(nodeid, time.day)
filter(log.merge.interior, hamabot > 7E3) %>% select(nodeid, time.day)
filter(log.merge, hamabot > 7E3) %>% select(nodeid, time.day)
unique( filter(log.merge, nodeid == 78) %>% select(time.day) )

# ====== clean log (operations)
# Clean for negative humidities (and NA humidies, none present in log.merge)
node.bad.all <- unique( filter(log.merge, !(humid_temp > 0)) %>% select(nodeid) )
# Clean for NA's in humidity and temperature (node 15)
node.bad.days<- unique( filter(log.merge, is.na(humidity) | is.na(humid_temp)) %>% 
                          select(nodeid, time.day) )
# Clean for bad HAMATOP points (day 314)
node.bad.days<- rbind(node.bad.days,
                      unique( filter(log.merge, time.day < 315) %>%
                          select(nodeid,time.day)) )

# Trim data that goes partway through day 343 (day of death), except in nodes that carry on.
time.day.last <- 343
node.good.long <- unique( filter(log.merge, time.day == 344) %>% select(nodeid) )

# Apply cleaning filters
log.merge <- filter(log.merge, (time.day < time.day.last) | 
                      (nodeid %in% node.good.long$nodeid))
log.merge <- anti_join(log.merge, node.bad.all , by=c('nodeid'))
log.merge <- anti_join(log.merge, node.bad.days, by=c('nodeid', 'time.day'))

log.merge.interior <- filter(log.merge, tree=='interior')
log.merge.edge <- filter(log.merge, tree=='edge')



# ================================================
# ANALYZE PAR
# ================================================



# Find the days with peak consistent activity
group_by(log.merge, nodeid) %>% summarise(n())
day.hist <- group_by(log.merge, time.day) %>% summarise(n=n())
peak.days.time.day <- filter(day.hist,n==8352)$time.day
peak.days <- group_by(log.merge, nodeid) %>%
  filter(time.day %in% peak.days.time.day)
summarise(peak.days, n=n())
peak.days.nodeid <- unique(peak.days$nodeid)
# Filter to peak-day nodes
locs.directions <- filter(locs, nodeid %in% peak.days.nodeid) %>%
  group_by(direction) %>%
  summarise(n=n())
# Compute suntime, the amount of sun averaged over the same time of day on different days
suntime <- filter(peak.days, direction=='SW' | direction=='WSW') %>%
  group_by(height.m, time.dmin, tree, distance) %>%
  summarise(light.total.top=sum(hamatop),light.total.bot=sum(hamabot)) %>%
  filter(!is.na(light.total.top) & !is.na(light.total.bot))

ggplot(suntime) +
  geom_line(aes(x=time.dmin, y=log(light.total.bot+1) + height.m, group=height.m, color=height.m))  

# Verify that all nodes have taken the same number of measurements over these "peak days"
ggplot(locs.directions, aes(x=direction, y=n, color='nodes active on peak day')) +
  geom_point()+
  geom_point(data = group_by(locs,direction) %>% summarise(n=n()), 
             aes(x=direction, y=n, color='all nodes'))

# Total daily incident PAR vs height
ggplot(filter(suntime, distance=='0.1')  %>% group_by(height.m,distance,tree) 
       %>% summarise(light.total.top=mean(light.total.top),
                     light.total.bot=mean(light.total.bot)) ) +
  geom_line(aes(x=height.m,y=light.total.top,color=tree,group=tree)) +
  ggtitle("Total daily incident PAR, avg of three days near Nov 20") +
  scale_y_log10()

# Total daily reflected PAR vs height
ggplot(filter(suntime, distance=='0.1')  %>% group_by(height.m,distance,tree) 
       %>% summarise(light.total.top=mean(light.total.top),
                     light.total.bot=mean(light.total.bot)) ) +
  geom_line(aes(x=height.m,y=light.total.bot,color=tree,group=tree)) +
  ggtitle("Total daily reflected PAR, avg of three days near Nov 20") +
  scale_y_log10()

# Tree plot: Total daily incident PAR
ggplot(filter(suntime, tree=='edge') %>% group_by(height.m,distance,tree) 
       %>% summarise(light.total.top=mean(light.total.top)) ) +
  geom_point(aes(x=distance, y=height.m,color=light.total.top)) +
  ggtitle("Incident PAR with height, three days near Nov 20")

# Tree plot: Total daily reflected PAR
ggplot(group_by(suntime,height.m,distance) 
       %>% summarise(light.total.bot=mean(light.total.bot)) ) +
  geom_point(aes(x=distance,y=height.m,color=log10(light.total.bot))) +
  ggtitle("Reflected PAR with height, three days near Nov 20")

ggplot(suntime) +  
  geom_point(aes(x=height.m,y=light.total.bot,color=tree,group=tree)) +
  ggtitle("Reflected PAR with height, three days near Nov 20")

ggplot(filter(peak.days,height.m==20)) +
  geom_point(aes(x=time.dmin+time.day*1440,y=is.na(hamatop))) +
  geom_point(aes(x=time.dmin+time.day*1440,y=is.na(hamabot)))

filter(peak.days, nodeid==20) %>%
  summarise(sum(hamatop))

ggplot(filter(log.merge, nodeid == 4, 0 < time.dmin, time.dmin < 1440), 
       aes(x=(time.dmin / 60),y=humid_temp,group=time.day,colour=time.day)) +
  geom_line()








# ==========================================================================================

# Give each row an identifier that specifies which dataset it came from
net$dset <- 'net'
net$result_time <- as.POSIXlt(net$result_time)
# Convert time from POSIXlt to POSIXct
net$time <- as.POSIXct(net$result_time) + 300 * net$epoch
# Merge location data into log
net.merge <- left_join(net, locs, by=c('nodeid'))
net.missing <- anti_join(net,locs, by=c('nodeid'))

unique(log.merge$nodeid)
unique(log.missing$nodeid)
unique(net.merge$nodeid)
unique(net.missing$nodeid)

unique(anti_join(unique(net.missing$nodeid), unique(log.missing$nodeid)))
unique(anti_join(unique(net.merge$nodeid), unique(log.merge$nodeid)))

#all <- rbind_list(select(net,nodeid,epoch,dset,humid_temp,humidity,hamatop,hamabot,voltage,result_time), 
#                  select(log,nodeid,epoch,dset,humid_temp,humidity,hamatop,hamabot,voltage,result_time)) %>%
#       arrange(nodeid,epoch)
#all.dset.n <- summarise(group_by(all, nodeid,epoch,dset),copies.dset=n())
#all.n <- summarise(group_by(all, nodeid, epoch), copies.total=n())
#all <- inner_join(all, all.dset.n, by=c('nodeid','epoch','dset'))
#all <- inner_join(all, all.n, by=c('nodeid','epoch'))

group_by(log,dset) %>% summarise(n=n())

# Checking voltage plots for errors in time
ggplot(filter(log.merge,epoch < 10000, voltage > 2), aes(x=time,y=voltage,group=nodeid,colour=nodeid)) +
  geom_line() + 
  ggplot(filter(log.merge, )
  geom_text(label=log.merge$nodeid)

ggplot(filter(all, dset=='log' & epoch < 4000 & humid_temp > 0), 
       aes(x=time,y=humid_temp,group=height.m,colour=height.m)) +
  geom_line()

ggplot(all.dset.n, aes(n)) +
  geom_bar(width=.5)


all.sd <- summarise(group_by(all,nodeid,epoch), 
                         humid_temp.sd=sd(humid_temp),
                         humidity.sd=sd(humidity),
                         hamatop.sd=sd(hamatop),
                         hamabot.sd=sd(hamabot),
                         voltage.sd=sd(voltage)) %>%
  filter(humid_temp.sd > 0 | humidity.sd > 0 |
           hamatop.sd > 0 | hamabot.sd > 0) %.% na.omit()

all.dset.sd <- summarise(group_by(all,nodeid,epoch,dset), 
                    humid_temp.sd=sd(humid_temp),
                    humidity.sd=sd(humidity),
                    hamatop.sd=sd(hamatop),
                    hamabot.sd=sd(hamabot),
                    voltage.sd=sd(voltage)) %>%
          filter(humid_temp.sd > 0 | humidity.sd > 0 |
                   hamatop.sd > 0 | hamabot.sd > 0) %.% na.omit()


ggplot(filter(all.sd, humid_temp.sd > 0 | humidity.sd > 0 |
                      hamatop.sd > 0 | hamabot.sd > 0) %.% na.omit(), 
       aes(x=humid_temp.sd, y=humidity.sd)) +
  geom_point()

ggplot(filter(all.sd, humid_temp.sd > 0 | humidity.sd > 0 |
                hamatop.sd > 0 | hamabot.sd > 0) %.% na.omit(), 
       aes(x=hamatop.sd, y=hamabot.sd, color= (hamabot.sd == 0 | hamatop.sd == 0))) +
  geom_point(position="jitter")

  
all.equal(net, log, ignore_row_order=TRUE)

