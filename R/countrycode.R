countrycode<-
function (SOURCEVAR, ORIGIN, DESTINATION) 
{
	if (ORIGIN == "country.name") {
		# Prepare target
		TARGET<-NULL
		for (z in 1:length(SOURCEVAR)){TARGET<-c(NA,TARGET)}
		# For each regex in the database -> find matches
		for (i in 1:nrow(countrycode_data)){
			Origin_code <- countrycode_data$regex[i]
			Destination_code <- countrycode_data[i,DESTINATION]
			matches <- as.vector(grep(Origin_code, as.vector(SOURCEVAR), perl = TRUE, ignore.case = TRUE, value = FALSE))
			# For each match -> replace in target vector
			for (j in matches) {
				TARGET[j] <- Destination_code
			}
		}
		TARGET
		
	}
	else {
		TARGET <- NA
		matchingmatrix <- countrycode_data[c(ORIGIN, DESTINATION)]
		matchingmatrix <- matchingmatrix[which(is.na(matchingmatrix[ORIGIN]) == 
								FALSE & is.na(matchingmatrix[DESTINATION]) == FALSE), 
		]
		Origin_code <- as.matrix(matchingmatrix[ORIGIN])
		Destination_code <- as.matrix(matchingmatrix[DESTINATION])
		matches <- match(SOURCEVAR, Origin_code)
		for (i in 1:length(SOURCEVAR)) {
			ifelse(is.na(matches[i]), NA, TARGET[i] <- Destination_code[matches[i]])
		}
		TARGET
	}
}
