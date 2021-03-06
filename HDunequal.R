#######################################################################################
# Hausdorff Distance (HD) for a set of vectors with differing numbers of observations #
#######################################################################################
# Input a list of vectors

HDunequal <-	function(alist) {
	require(pracma)
	
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

			# Calculate the Hausdorff distance for the lower triangular of the matrix
			if(j < i){
				InnerVector[j] <- hausdorff_dist(as.matrix(alist[[i]]), as.matrix(alist[[j]]))
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