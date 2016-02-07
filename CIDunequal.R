###################################################################################################
# Complexity Invariant Distance (CID) for a set of vectors with differing numbers of observations #
###################################################################################################
# Input a list of vectors

CidMatrix <-	function(alist) {
	
	# Define the Complexity Invariant Distance (CID) function
	cid <- function(x,y) {
		cesx <- sqrt(sum(diff(x)^2))
		cesy <- sqrt(sum(diff(y)^2))
		denom <- min(cesx, cesy)
		if(denom == 0) {stop("Cannot divide by zero: A series exists that has complexity zero.")}
		else if(denom > 0){cid1 <- max(cesx, cesy)/denom 
		return(cid1)
	}

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
				InnerVector[j] <- cid(alist[[i]], alist[[j]])
			}
		}

		# Save the output from the inner part of the nested loop
		OuterMatrix[,i] <- InnerVector

		# Print the progress in terms of i
		print(i)	
		gc()	
	}
	
	# Get the full dissimilarity matrix
	DissimilarityMatrix <- OuterMatrix + t(OuterMatrix)

	#Add names to the rows and columns
	rownames(DissimilarityMatrix) <- names(alist)
	colnames(DissimilarityMatrix) <- names(alist) 

	return(DissimilarityMatrix)
}