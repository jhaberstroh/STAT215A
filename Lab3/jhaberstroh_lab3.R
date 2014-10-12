library('doParallel')

wd <- '/home/jhaberstroh/Courses/STAT215/REPO/Lab3'
setwd(wd)
load('lingBinary.RData')
# Loads object lingBinary
# Assuming shape  ~(45k, 474) with non-answer columns stored as lingBinary.identifiers

# Uncomment the following to check variable properties:
#ls()
#dim(lingBinary)
#names(lingBinary)

lingBinary.identifiers <- c("ID", "CITY", "STATE", "ZIP", "lat")


# ClusterSubsample takes a dataframe, a fraction, and a clutstering method.
#	data: 			A dataframe of arbitrary size (n,k). No checking is done internally.
#				This function will generates O(n^2) data
#	sub.fraction: 		A float specifying the fraction of rows to cluster
#	cluster.method:		The clustering method to be applied. Must return a vector of length n with
#				discrete identifiers for cluster belonging.
ClusterSubsample.mtx <- function(data, sub.fraction, cluster.method){

}
