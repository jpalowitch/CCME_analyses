slpaRead <- function (fin) {

	conn <- file(fin, open="r")
	linn <- readLines(conn)
	osOut <-  list(NULL)
	for (i in 1:length(linn)) {
	    readIn <- linn[i]
	    readIn <- strsplit(readIn, " ")[[1]]
	    if (!((readIn[1] == "#module"))) {
	        readIn <- sapply(readIn, as.numeric)
	        names(readIn) <- NULL
	        osOut <- c(osOut, list(readIn))
	    }
	}
	close(conn)
	osOut <- osOut[-1]
	
	# Finding singleton communities
	bglocs <- which(unlist(lapply(osOut, length)) == 1)
	if (length(bglocs) > 0) {
	  bg <- unique(unlist(osOut[bglocs]))
	  osOut <- osOut[-bglocs]
	} else {
	  bg <- integer(0)
	}
	
	osOutFormat <- list("communities" = osOut,"background" = bg)
	return(osOutFormat)

}