## ---------------------------
## Script name: HarborCheck.R
##
## Purpose of script: To check whether a fishing vessel is within a
##  specified buffer distance of its homeport or not
##
## Date Created: 2022-10-18
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
#' HarborCheck
#'
#' This function checks to see if data fall within a specified buffer zone
#' around a point of interest (typically the homeport of a vessel).
#' @param homeport a vector containing the longitude and latitude of the
#' homeport. Longitude and latitude should be specified in decimal degrees in
#' the order c(lon, lat). There is no default value.
#' @param dataframe a dataframe with at least the following columns LAT and LON.
#' LAT and LON should be in decimal degrees. There is no default.
#' @param buffer a single numeric value representing the size of the buffer
#' (radius) around the homeport. There is no default value.
#' @param uom a character value representing the unit of measure that the buffer
#' is specified in. Defaults to 'meters'. Other acceptable values include
#' 'kilometers', 'nautical_miles', and 'miles'.
#' @returns Returns the original dataframe with two additional columns called
#' "dist_from_port" and "inPort".
#' "dist_from_port" contains a numeric value representing the straight line
#' distance from the port to the data point in uom specified by the user.
#' "inPort" contains a TRUE/FALSE value describing whether the points in the
#' data occur inside the user-defined harbor buffer (TRUE) or not.
#' @examples
#' df=data.frame(
#'   LAT=c(42.16915,42.16916),
#'   LON=c(-66.92022,-66.92025),
#'   TIMESTAMP=c('2022-10-24 10:00:30','2022-10-24 10:05:30'),
#'   temperature=c(10,10.2),
#'   depth=c(100,101)
#'   )
#' HarborCheck(
#'      homeport=c(-70.92033,41.63875),
#'      dataframe=df,
#'      buffer=1,
#'      uom='nautical_miles'
#'      )
HarborCheck=function(homeport,dataframe,buffer,uom='meters'){
  ## Calculate the distance from port to the lat/lon in meters
  dataframe$dist_from_port=base::by(
    dataframe,
    1:nrow(dataframe),
    function(row){
      geosphere::distHaversine(homeport,c(row$LON,row$LAT))
    }
  )
  ## Convert the distance to the specified unit of measure
  if(uom!='meters'){
    if(uom=='kilometers'){
      dataframe$dist_from_port=dataframe$dist_from_port/1000
    } else {
      if(uom=='nautical_miles'){
        dataframe$dist_from_port=dataframe$dist_from_port*0.000539957
      } else {
        if(uom=='miles'){
          dataframe$dist_from_port=dataframe$dist_from_port*0.00062137141841645
        }
      }
    }
  }
  ## Return whether the distance is within the specified buffer or no
  dataframe$inPort=dataframe$dist_from_port<buffer
  return(dataframe)
}
