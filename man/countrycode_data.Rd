\name{countrycode_data}
\alias{countrycode_data}
\docType{data}
\title{Translation data frame used by the countrycode function}
\description{
 Translation dataframe for use with the countrycode package
}
\usage{data(countrycode_data)}
\format{
  A data frame with 265 observations on the following 11 variables.
  }
\source{

http://www.iso.org/iso/country_codes/iso_3166_code_lists

http://unstats.un.org/unsd/methods/m49/m49regin.htm

http://www.state.gov/s/inr/rls/4250.htm

http://www.correlatesofwar.org/

}
\note{
To produce consistent conversion, some entries had to be removed from the conversion data frame. For example, the Correlates of War include 4 different codes to represent Western Germany. "countrycode" uses only one of them (CoW code 260 for all years). Similar choices were made in the cases of Korea, Yemen, Congo and Vietnam. Also, Namibia's iso2c code ("NA") can be understood as a missing observation (NA) by R.

Capitalized country names refer to the official short English names, as defined by the ISO organization. ISO does not publish official short English names for countries whose name is not capitalized in the the country.name vector.

Here is a list of countries and codes that were dropped intentionally:

KOREA, REPUBLIC OF 
cown: 731, 730
cowc: PRK, KOR

YEMEN
cown: 680, 678
cowc: YAR, YPR

GERMANY
cown: 267, 255, 245
cowc: BAV, GMY, BAD

CONGO
cown: 490
cowc: 484

VIET NAM
cown: 816
cowc: DRV

}
\examples{
## No example necessary. Data is set to LazyLoad
}
\keyword{datasets}
