library(ggplot2)

# ---------------------------------------------------------------------
# BEGIN data load
# ---------------------------------------------------------------------
# Load and process data 
data(USArrests)
state.coord <- read.table("~/Code/STAT215/lab0/stateCoord.txt", strip.white=TRUE)

# ---------------------------------------------------------------------
# BEGIN data cleaning
# ---------------------------------------------------------------------
# Validate that names match on corresponding rows
state.coord[,'arrestName'] <- row.names(USArrests)
which(state.coord[,'arrestName'] != row.names(state.coord))
state.coord[which(state.coord[,'arrestName'] != row.names(state.coord)),c('long','arrestName')]

# Overwrite row names with arrest names
row.names(state.coord) <- state.coord[,'arrestName']
state.data <- merge(state.coord, USArrests, by='row.names')

# Flag outlier data for color
state.data[,"outlier"] <- 0
state.data[which(state.data[,"Row.names"] == "Alaska"),"outlier"] <- 1

# ---------------------------------------------------------------------
# BEGIN plotting
# ---------------------------------------------------------------------
# 2.1: Plotting (Murder vs Assault)
ggplot(state.data, aes(x=Murder,y=Assault)) + geom_point()

# 2.2: Plotting (Rape vs UrbanPop) with outlier colored, using Row.names
ggplot(state.data, aes(x=Rape, y=UrbanPop, label=Row.names)) + 
  geom_point(data=state.data[which(state.data[,"Row.names"] != "Alaska"),]) +
  geom_point(data=state.data[which(state.data[,"Row.names"] == "Alaska"),], colour="#DD0000") + 
  ylim(0,95) + xlim(0,50)

# 2.2: Plotting (Rape vs UrbanPop) with outlier colored, using outlier column
ggplot(state.data, aes(x=Rape, y=UrbanPop, label=Row.names, color=outlier)) + 
  geom_point(data=state.data[which(state.data[,"Row.names"] != "Alaska"),]) +
  ylim(0,95) + xlim(0,50)

# 2.3: Plotting (Rape vs UrbanPop) with state labels
ggplot(state.data, aes(x=Rape, y=UrbanPop, label=Row.names)) + 
  geom_point() + 
  geom_text(size=3, hjust=0, vjust=0)

# 3.1: Linear regression, including Alaska
ggplot(state.data, aes(x=Rape, y=UrbanPop, label=Row.names)) + 
  geom_point(data=state.data[which(state.data[,"Row.names"] != "Alaska"),]) +
  geom_point(data=state.data[which(state.data[,"Row.names"] == "Alaska"),], colour="#CC0000") +
  geom_smooth(method='lm', formula = y ~ x)

# 3.1: Linear regression, excluding Alaska
ggplot(state.data, aes(x=Rape, y=UrbanPop, label=Row.names)) + 
  geom_point(data=state.data[which(state.data[,"Row.names"] != "Alaska"),]) +
  geom_point(data=state.data[which(state.data[,"Row.names"] == "Alaska"),], colour="#CC0000") +
  geom_smooth(method='lm', formula = y ~ x, data=state.data[which(state.data[,"Row.names"] != "Alaska"),])


# ---------------------------------------------------------------------
# Extra: Plotting data with point size, scattered by lat/long
ggplot(state.data, aes(x=long,y=lad,label=Row.names,size=UrbanPop, color=Rape/UrbanPop)) + 
  geom_point() + xlim(-125, -70) + ylim(10,70) +
  ggtitle("Rape per urban capita, USA")
