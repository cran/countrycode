`countrycode`<-function (SOURCEVAR, ORIGIN, DESTINATION) 
{
   	if (ORIGIN == "country.name") {
		TARGET<-NA
		for (i in 1:nrow(countrycode_data)) {
            Origin_code <- as.character(countrycode_data$regex[i])
			Destination_code<-as.matrix(countrycode_data[DESTINATION])
            matches <- grep(Origin_code, SOURCEVAR, perl = TRUE, ignore.case = TRUE, value = FALSE)
            for (j in matches) {
            	TARGET[j] <- as.character(Destination_code[i])
			}
        }
		TARGET
	}
    else {
	TARGET<-NA
        matchingmatrix <- countrycode_data[c(ORIGIN, DESTINATION)]
        matchingmatrix <- matchingmatrix[which(is.na(matchingmatrix[ORIGIN]) == FALSE & is.na(matchingmatrix[DESTINATION]) == FALSE), ]
        Origin_code <- as.matrix(matchingmatrix[ORIGIN])
        Destination_code <- as.matrix(matchingmatrix[DESTINATION])
        matches <- match(SOURCEVAR, Origin_code)
        for (i in 1:length(SOURCEVAR)) {
            ifelse(is.na(matches[i]), NA, TARGET[i] <- Destination_code[matches[i]])
        }
	TARGET
    }
}
