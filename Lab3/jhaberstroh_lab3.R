library('doParallel')
library('foreach')
library('rlecuyer')

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
lingBinary.answers <- names(lingBinary)[!names(lingBinary) %in% lingBinary.identifiers]

# kMeansWrap: Wrapper for kmeans
kMeansWrap <- function(data, k){
	clusters <- kmeans(data, k)
	return(clusters$cluster)
}

# ClusterSubsample: Peform clustering on subsampling of the data
#	data: 			A dataframe of arbitrary size (n,k). No checking is done internally.
#				This function will generates O(n^2) data
#	sub.fraction: 		A float specifying the fraction of rows to cluster
#	cluster.method:		The clustering method to be applied.
#		input: 	data = dataframe size (n,k)
#			k = integer, number of clusters
#		output: Vector of length n with discrete identifiers for cluster belonging (i.e. a,b,c or 1,2,3)
#	cluster.k:		Number of clusters to use for cluster method
ClusterSubsample.mtx <- function(data, sub.fraction, cluster.method, cluster.k){
	sample.1 <- sort(sample(nrow(data), nrow(data) * sub.fraction, replace=FALSE))
	sample.2 <- sort(sample(nrow(data), nrow(data) * sub.fraction, replace=FALSE))
	cluster.1 <- cluster.method(data[sample.1,], cluster.k)
	cluster.2 <- cluster.method(data[sample.2,], cluster.k)
	
	# Sorted and replace==FALSE, so indices will align after subsetting
	intersect.1 <- sample.1 %in% sample.2
	intersect.2 <- sample.2 %in% sample.1

	print(head(intersect.1))
	print(length(cluster.1))
	print(length(intersect.1))
	print("Cluster intersection")
	print(head(cluster.1[intersect.1]))
	print(head(cluster.2[intersect.2]))

	C.1 <- outer(cluster.1[intersect.1],cluster.1[intersect.1], FUN = '==')
	C.2 <- outer(cluster.2[intersect.2],cluster.2[intersect.2], FUN = '==')
	print("The similarity mtxes")
	print(C.1)
	print(C.2)
	print(C.1 * C.2)
	N11 <- sum(sum(C.1 * C.2))
	
	#return(sum(C.1, c(1,2)))
	return(N11)
}

ClusterSubsample.mtx(lingBinary[,lingBinary.answers], .01, kMeansWrap, 4)
