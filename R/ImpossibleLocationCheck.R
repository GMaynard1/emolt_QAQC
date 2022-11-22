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
#' @param dataframe a dataframe containing at least the columns LATITUDE and LONGITUDE
#' @returns Returns the original dataframe with an extra column called ImpossibleLocation in which TRUE indicates bad data.
#' @examples
#' ImpossibleLocationCheck(
#'   dataframe=df
#'   )
#' @export
ImpossibleLocationCheck=function(dataframe){
  dataframe$ImpossibleLocation=ifelse(
    -90>dataframe$LATITUDE|dataframe$LATITUDE>90|-180<dataframe$LONGITUDE|180>dataframe$LONGITUDE,
    TRUE,
    FALSE
  )
  return(dataframe)
}
