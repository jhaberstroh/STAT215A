library('doParallel')
library('foreach')
library('rlecuyer')
library('Rcpp')
library('microbenchmark')
library('ggplot2')

print("Loading data...")
# Set directory of lingBinary.Rdata as wd and directory for ouput as save_dir
wd <- "/home/jhaberstroh/Courses/STAT215/Lab3"
save_dir <- "/home/jhaberstroh/Dropbox/Public"
setwd(wd)
load(paste0(wd, '/', 'lingBinary.RData'))
# Assuming shape ~(45k, 474) with non-answer columns stored as lingBinary.identifiers

sourceCpp(file.path(wd, 'similarity.cpp'))

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

# Jaccard.mtx: Computes Jaccard similarity using a matrix formalism
Jaccard.mtx <- function(data.1, data.2){
	C.1 <- outer(data.1,data.1, FUN = '==')
	C.2 <- outer(data.2,data.2, FUN = '==')
	#print("Overlapping similarity matrix")
	#print(C.1 * C.2)
	C.1.2 <- sum(sum(C.1 * C.2)) 
	J <- C.1.2 / (sum(sum(C.1)) + sum(sum(C.2)) - C.1.2)
	return(J)
}

# ClusterSubsample: Peform clustering on subsampling of the data
#	data: 			A dataframe of arbitrary size (n,k). No checking is done internally.
#				This function will generates O(n^2) data
#	sub.fraction: 		A float specifying the fraction of rows to cluster
#	cluster.method:		The clustering method to be applied.
#		input: 	dataframe size (n,k)
#			integer, number of clusters
#		output: Vector of length n with integer numeric identifiers compatible with similarity.method
#	cluster.k:		Number of clusters to use for cluster method
#	similarity.method: 	Method taking two objects output from cluster.method, returning a double
ClusterSubsample <- function(data, sub.fraction, cluster.method, cluster.k, similarity.method){
        # Grab two subsamples and cluster them
	sample.1 <- sort(sample(nrow(data), nrow(data) * sub.fraction, replace=FALSE))
	sample.2 <- sort(sample(nrow(data), nrow(data) * sub.fraction, replace=FALSE))
	cluster.1 <- cluster.method(data[sample.1,], cluster.k)
	cluster.2 <- cluster.method(data[sample.2,], cluster.k)
	
	# Select the overlap of the two samples to perform intersection
	intersect.1 <- sample.1 %in% sample.2
	intersect.2 <- sample.2 %in% sample.1

	return(similarity.method(cluster.1[intersect.1], cluster.2[intersect.2]))
}



SimilarityProfile <- function(data, sub.fraction, cluster.method, cluster.k, similarity.method){
	print("Sample size:")
	print(as.integer(nrow(data) * sub.fraction))
	sample.1 <- sort(sample(nrow(data), nrow(data) * sub.fraction, replace=FALSE))
	sample.2 <- sort(sample(nrow(data), nrow(data) * sub.fraction, replace=FALSE))
	cluster.1 <- cluster.method(data[sample.1,], cluster.k)
	cluster.2 <- cluster.method(data[sample.2,], cluster.k)
	
	# Sorted and replace==FALSE, so indices will align after subsetting
	intersect.1 <- sample.1 %in% sample.2
	intersect.2 <- sample.2 %in% sample.1

	benchmarks <- microbenchmark(similarity.method(cluster.1[intersect.1], cluster.2[intersect.2]),
				    	Jaccard.mtx(cluster.1[intersect.1], cluster.2[intersect.2]), 
					times=5)
	print(benchmarks)
	print("Check that answers match")
	print(similarity.method(cluster.1[intersect.1], cluster.2[intersect.2]))
	print(Jaccard.mtx(cluster.1[intersect.1], cluster.2[intersect.2]))

	return(0)
}


## Perform the matrix version on ~5k entries
#ClusterSubsample(lingBinary[,lingBinary.answers], .11, kMeansWrap, k, similarity.method = Jaccard.mtx)

# Perform the optimized c++ version in parallel
registerDoParallel(cores=8)
n_jobs <- 500
RNGkind("L'Ecuyer-CMRG")
for (k in seq(2,7)){
    print(sprintf("Performing %d simulations with k=%d...", n_jobs, k))
    sims <- foreach(i = 1:n_jobs) %dopar% ClusterSubsample(lingBinary[,lingBinary.answers], .8, kMeansWrap, k,
    	 					    similarity.method = JaccardCPP)
    sims <- unlist( lapply(sims[1:n_jobs], as.numeric) )
    sims_df <- data.frame(sim_value=sims)
    print(sims_df)
    qplot(sim_value, data=sims_df, geom="histogram", xlim=c(0,1.01), binwidth=.01) +
        ggsave(file=paste0(save_dir, "/", sprintf("STAT215-Lab3-k%d.pdf", k)))
}

