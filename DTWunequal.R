#################################################################################
# Dynamic Time Warping (DTW) for vectors with differing numbers of observations #
#################################################################################
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

		# Print the progress in terms of i
		print(i)	

		gc()	
	}
	
	# Unlist the outer list to get the dissimilarity matrix
	DissimilarityMatrix <- OuterMatrix + t(OuterMatrix)

	#Add names to the rows and columns
	rownames(DissimilarityMatrix) <- names(alist)
	colnames(DissimilarityMatrix) <- names(alist) 

	return(DissimilarityMatrix)
}