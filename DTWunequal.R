##########################################################################################
# Dynamic Time Warping (DTW) for a set of vectors with differing numbers of observations #
##########################################################################################
# Input a list of vectors

DtwMatrix <-	function(alist) {
	require(dtw)

	# Get the length of the list
	N <- length(alist)

	# Preallocate the outer loop matrix to hold the results from the inner loop
	OuterMatrix <- matrix(0, N, N)
	
	# Preallocate the inner vector list to hold the results from the inner loop		
	InnerVector <- rep(0, N)

	# Start the outer part of the nested loop
	for (i in 1:N) {
						
		# Start the inner loop
		for(j in 1:i) {

			# Calculate the DTW distance for the lower triangular of the matrix
			if(j < i){
				InnerVector[j] <- dtw(alist[[i]], alist[[j]], distance.only=TRUE)$distance
			}
		}

		# Save the output from the inner part of the nested loop
		OuterMatrix[,i] <- InnerVector
	}
	
	# Get the full dissimilarity matrix
	DissimilarityMatrix <- OuterMatrix + t(OuterMatrix)

	#Add names to the rows and columns
	rownames(DissimilarityMatrix) <- names(alist)
	colnames(DissimilarityMatrix) <- names(alist) 

	return(DissimilarityMatrix)
}

# Generate Time Series Data
set.seed(42);
randomTS <- function(n,f){
	u=sort(runif(n)*2*pi)
	c=runif(1)
	y=c*f(u)+rnorm(n)/4
	df=data.frame(x=u,y=y)
	plot(df, type = "l")
}

N <- 50
TsList <- list()
for(i in 1:N){
	if(round(runif(1)) == 0){TsList[[i]] <- randomTS(1000,sin)}
	if(round(runif(1)) == 1){TsList[[i]] <- randomTS(1000,cos)}
}
