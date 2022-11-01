## ---------------------------
## Script name: ImpossibleLocationCheck.R
##
## Purpose of script: To check whether the recorded location is out of bounds
##
## Date Created: 2022-10-21
##
## Software code created by U.S. Government employees is
## not subject to copyright in the United States
## (17 U.S.C. §105).
##
## Email: george.maynard@noaa.gov
##
## ---------------------------
## Notes:
##
##
## ---------------------------
#' ImpossibleLocationCheck
#'
#' This function checks to see if the location of the data collection is out of
#' bounds
#' @param lat latitude of the data collection. There is no default value.
#' @param lon longitude of the data collection. There is no default value.

#' @returns Returns a TRUE/FALSE value as to whether location is out of bounds.
#' TRUE indicates bad data.
#' @examples
#' ImpossibleLocationCheck(
#'   lat=42.16915,
#'   lon=-66.92022
#'   )

ImpossibleLocationCheck=function(lat,lon){
  ## Check if the values are out of range
  if(-90<lat&lat<90&-180<lon&lon<180){
    return(FALSE)
  } else {
    return(TRUE)
  }
}
