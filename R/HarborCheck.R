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
#' @param homeport a vector containing the longitude and latitude of the homeport. Longitude and latitude should be specified in decimal degrees in the order c(lon, lat). There is no default value.
#' @param lon a single numeric value representing the longitude where the observation was collected. Longitude should be specified in decimal degrees. There is no default value
#' @param lat a single numeric value representing the latitude where the observation was collected. Latitude should be specified in decimal degrees. There is no default value
#' @param buffer a single numeric value representing the size of the buffer (radius) around the homeport. There is no default value.
#' @param uom a character value representing the unit of measure that the buffer is specified in. Defaults to 'meters'. Other acceptable values include 'kilometers', 'nautical_miles', and 'miles'.

#' @returns Returns a TRUE/FALSE value as to whether the point specified by lat, lon is within the buffer around the point specified by homeport.
#' @examples
#' HarborCheck(
#'      homeport=c(-70.92033,41.63875),
#'      lon=-66.92022,
#'      lat=42.16915,
#'      buffer=250,
#'      uom='nautical_miles'
#'      )
HarborCheck=function(homeport,lon,lat,buffer,uom='meters'){
  ## Calculate the distance from port to the lat/lon in meters
  dist_from_port=geosphere::distHaversine(
    p1=homeport,
    p2=c(lon,lat)
  )
  ## Convert the distance to the specified unit of measure
  if(uom!='meters'){
    if(uom=='kilometers'){
      dist_from_port=dist_from_port/1000
    } else {
      if(uom=='nautical_miles'){
        dist_from_port=dist_from_port*0.000539957
      } else {
        if(uom=='miles'){
          dist_from_port=dist_from_port*0.00062137141841645
        }
      }
    }
  }
  ## Return whether the distance is within the specified buffer or no
  return(dist_from_port < buffer)
}
