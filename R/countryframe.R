`countryframe` <-
function(TYPE,CODE,YEARS){
  countries <- as.vector(countrycode_data[,c(CODE)])
  countries <- countries[which(is.na(countries) == FALSE)]
  countries <- as.vector(sort(countries))
  id <- 0
  years <- YEARS   
if(TYPE=="directed.dyads"){
  as.data.frame(do.call("rbind", lapply(countries, function(country1){
    do.call("rbind", lapply(countries, function(country2){
      id <<- id + 1
      do.call("rbind", lapply(years, function(year){
        if(country1 != country2){
          cbind(id, country1,country2,year)
        }}))}))})))
} else if (TYPE=="country.years"){
  as.data.frame(do.call("rbind", lapply(countries, function(country){
    do.call("rbind", lapply(years, function(year){
      cbind(country,year)
    }))})))
} else if (TYPE=="undirected.dyads"){
    as.data.frame(do.call("rbind", lapply(countries, function(country1){
    do.call("rbind", lapply(countries, function(country2){
      id <<- id + 1
      do.call("rbind", lapply(years, function(year){
        if(country1 != country2 & country1<country2){
          cbind(id, country1,country2,year)
        }}))}))})))
} else {print("The three valid types of dataframes are: undirected.dyads, directed.dyads, and country.years. The arguments must be enclosed in double quotes.")}
}

