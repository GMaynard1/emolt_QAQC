## ---------------------------
## Script name: GearSpeedCheck.R
##
## Purpose of script: To check whether the gear is moving too quickly for
##  fishing effort and should likely be considered transiting instead.
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
## Notes: Data from fixed gear vessels cannot be checked using this script
##
## ---------------------------
#' GearSpeedCheck
#'
#' This test is only applied to profiles coming from mobile gear. It controls
#' whether there are no erroneous locations provided. The speed of the vessels
#' are generated given the positions and times of the vessel. Vessel speed is
#' expected not to exceed 3 ms−1. Otherwise, it means either the positions or
#' times are bad data, or a vessel is sailing full speed rather than fishing.
#' This test is helpful for determining if there is an error in merging the
#' sensor and GPS data, often due to setting a sensor to a time zone other than
#' UTC.
#'
#' @param dataframe a dataframe with at least the following columns TIMESTAMP, LAT,
#' LON. TIMESTAMP should be formatted as yyyy-mm-dd HH:MM:SS. LAT and LON should
#' be in decimal degrees. Each data frame must have at least two rows of data.
#'
#' @param timezone a character string indicating the timezone associated with
#' the specified timestamp. Appropriate values can be found using the command
#' OlsonNames()
#' The default is UTC
#'
#' @param maxSpeed the maximum allowable speed. Defaults to 3 meters per second
#' (~ 8 knots) for all mobile gear.
#'
#' @param speedUnits the units that the maximum speed is specified in. Defaults
#' to 'mps' (meters per second), other options include 'mph' (miles per hour)
#' and 'kts' (knots).
#'
#' @param gearType a 2016 ISSCFG Gear Code or abbreviation (full list here: https://www.fao.org/3/bt988e/bt988e.pdf) There is no default value.

#' @returns Returns the original dataframe with an additional column called
#' "BadSpeed". The new column contains a TRUE/FALSE value describing whether the
#' points in the data occur at a bad (improbable) speed (TRUE)
#' @examples
#' df=data.frame(
#'   LAT=c(42.16915,42.16916),
#'   LON=c(-66.92022,-66.92025),
#'   TIMESTAMP=c('2022-10-24 10:00:30','2022-10-24 10:05:30'),
#'   temperature=c(10,10.2),
#'   depth=c(100,101)
#'   )
#'
#' GearSpeedCheck(
#'   dataframe=df,
#'   timezone='US/Eastern',
#'   maxSpeed=8,
#'   speedUnits='kts',
#'   gearType='OTB'
#'   )
GearSpeedCheck=function(dataframe,timezone,maxSpeed,speedUnits,gearType){
  ## Check to see whether the gear type is listed. If not, return an error.
  if(gearType%in%gears$ABBREVIATION){
    GEAR=gears[which(gears$ABBREVIATION==gearType),]
  } else {
    if(as.numeric(gearType)%in%gears$ISSCFG_CODE){
      GEAR=gears[which(gears$ISSCFG_CODE==as.numeric(gearType)),]
    } else {
      stop("Invalid gear type")
    }
  }
  if(GEAR$TYPE=="MOBILE"){
    ## Convert the timestamps to POSIX values
    x=dataframe
    x$TIMESTAMP=lubridate::ymd_hms(x$TIMESTAMP,tz=timezone)
    if(timezone!='UTC'){
      x$TIMESTAMP=lubridate::with_tz(x$TIMESTAMP,tzone="UTC")
    }
    ## Calculate the distance between each pair of points (meters and seconds)
    x$dist=0
    x$diffTime=0
    for(i in 2:nrow(x)){
      x$dist[i]=geosphere::distHaversine(
        p1=c(x$LON[i-1],x$LAT[i-1]),
        p2=c(x$LON[i],x$LAT[i])
      )
      x$diffTime[i]=difftime(x$TIMESTAMP[i],x$TIMESTAMP[i-1],units="secs")
    }
    ## Calculate the speed in meters / second
    x$speed=x$dist/x$diffTime
    ## Calculate the speed and convert as necessary
    if(speedUnits!='mps'){
      if(speedUnits=='mph'){
        x$speed=x$speed*2.23694
      } else {
        if(speedUnits=='kts'){
          x$speed=x$speed*1.94384
        } else {
          stop("Invalid speed unit")
        }
      }
    }
    dataframe$BadSpeed=x$speed>maxSpeed
  } else {
    dataframe$BadSpeed=FALSE
  }
  return(dataframe)
}
