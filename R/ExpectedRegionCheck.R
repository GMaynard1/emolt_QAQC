## ---------------------------
## Script name: ExpectedRegionCheck.R
##
## Purpose of script: To check whether a fishing vessel is within a
##  specified operating region or not.
##
## Date Created: 2022-10-19
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
#' ExpectedRegionCheck
#'
#' This function checks to see if the data fall within the expected
#' operating region of a vessel.
#' @param region the region the vessel is expected to be operating in. Can be
#' one of the following: 'Alaska','North_Atlantic','Greenland','New_Zealand',
#' 'North_Sea_Baltic'. There is no default value.
#' @param dataframe a dataframe containing at least the columns LATITUDE and LONGITUDE,
#' where LATITUDE is latitude in decimal degrees and LONGITUDE is longitude in decimal
#' degrees. There is no default value.

#' @returns Returns the original dataframe with an additional column called
#' "OutRegion". The new column contains a TRUE/FALSE value describing whether
#' the points in the data occur outside of the specified region (TRUE)
#' @examples
#' ExpectedRegionCheck(
#'      region='North_Atlantic',
#'      dataframe=df
#'      )
#' @export
ExpectedRegionCheck=function(region,dataframe){
  ## Grab the coordinates based on the region
  if(region=='Alaska'){
    minLon=-180
    maxLon=-125
    minLat=45
    maxLat=90
  }
  if(region=='North_Atlantic'){
    minLon=-75
    maxLon=30
    minLat=30
    maxLat=90
  }
  if(region=='Greenland'){
    minLon=-60
    maxLon=-15
    minLat=55
    maxLat=90
  }
  if(region=='New_Zealand'){
    minLon=160
    maxLon=175
    minLat=-50
    maxLat=-30
  }
  if(region=='North_Sea_Baltic'){
    minLon=-15
    maxLon=30
    minLat=45
    maxLat=60
  }
  dataframe$OutRegion=NA
  ## Check if the specified point is within the specified region
  dataframe$OutRegion=ifelse(
    dataframe$LONGITUDE<=maxLon&dataframe$LONGITUDE>=minLon&dataframe$LATITUDE<=maxLat&dataframe$LATITUDE>=minLat,
    FALSE,
    TRUE
    )
  return(dataframe)
}
