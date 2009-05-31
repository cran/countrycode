`countrycode` <-
function(SOURCEVAR, ORIGIN, DESTINATION){

TARGET <- NA

if (ORIGIN == "country.name"){
symmetric_vectors <- cbind(countrycode_data[DESTINATION], countrycode_data$regex)
symmetric_vectors <- symmetric_vectors[which (is.na(symmetric_vectors[1])==FALSE & is.na(symmetric_vectors[2])==FALSE ),]
Origin_Vector <- as.matrix(symmetric_vectors[2])

Destination_Vector <- as.matrix(symmetric_vectors[1])
for (i in 1:nrow(symmetric_vectors)){
regexpression <- as.character(Origin_Vector[i])
matches <- grep(regexpression, SOURCEVAR, perl = TRUE, ignore.case = TRUE, value=FALSE)
for (j in matches){
TARGET[j] <- as.character(Destination_Vector[i])
}
TARGET 
}
}

else{
symmetric_vectors <- countrycode_data[c(ORIGIN, DESTINATION)]
symmetric_vectors <- symmetric_vectors[which (is.na(symmetric_vectors[ORIGIN])==FALSE & is.na(symmetric_vectors[DESTINATION])==FALSE ),]
Origin_Vector <- as.matrix(symmetric_vectors[ORIGIN])

Destination_Vector <- as.matrix(symmetric_vectors[DESTINATION])

temp1 <- match(SOURCEVAR, Origin_Vector)

for (i in 1:length(SOURCEVAR)){

ifelse(is.na(temp1[i]), NA, TARGET[i] <- Destination_Vector[temp1[i]])

}
TARGET
}

}

