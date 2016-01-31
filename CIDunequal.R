##########################################################################################
# Complexity Invariant Distance (CID) for vectors with differing numbers of observations #
##########################################################################################
# Input a list of vectors

CidMatrix <-	function(alist) {
	
	# Define the Complexity Estimate (CE) function
	ces <- function(x) {
		M <- length(x)
		if(M <= 1) {ce2 <- 0}
		else if(M > 1) {
			ce1 <- rep(0,M) # Define a vector to hold the results
			N <- M-1
			K <- N-1
			for (n in 1:K){
				ce1[n] <- (x[n+1]-x[n])^2
			}
		ce2 <- sqrt(sum(ce1))
		}
	return(ce2)
	}

	# Define the Complexity Invariant Distance (CID) function
	cid <- function(x,y) {
		denom <- min(ces(x), ces(y))
		if(denom == 0){denom <- 1}
		cid1 <- max(ces(x), ces(y))/denom 
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
	
	# Unlist the outer list to get the dissimilarity matrix
	DissimilarityMatrix <- OuterMatrix + t(OuterMatrix)

	#Add names to the rows and columns
	rownames(DissimilarityMatrix) <- names(alist)
	colnames(DissimilarityMatrix) <- names(alist) 

	return(DissimilarityMatrix)
}