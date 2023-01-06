## ---------------------------
## Script name: GearTypeCheck.R
##
## Purpose of script: To check whether the distance traveled during the profile
##  by the sensor is appropriate for the type of gear deployed by the vessel
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
## Notes: The eMOLT program is unable to use this check for fixed gear currently
##    because only a single point is returned for fixed gear (i.e., no point is
##    recorded when gear is deployed)
##
## ---------------------------
#' GearTypeCheck
#' This function checks whether the distance traveled by the sensor during the
#' profile is appropriate for the type of gear deployed by the vessel. Distance
#' traveled is calculated between the first and last data locations.
#' Theoretically, fixed gear should not move very far from where it is deployed.
#' However, mobile gear should move a minimum distance (which will be fishery
#' dependent). For example, tows by draggers targeting groundfish may be
#' hundreds of meters while tows with hydraulic clam dredges may only be tens of
#' meters.
#' @param dataframe a dataframe with at least the following columns TIMESTAMP,
#' LAT, LON. TIMESTAMP should be formatted as yyyy-mm-dd HH:MM:SS. LAT and LON
#' should be in decimal degrees. Each data frame must have at least two rows of data.
#' @param gearType a 2016 ISSCFG Gear Code or abbreviation (full list here: https://www.fao.org/3/bt988e/bt988e.pdf) There is no default value.
#' @param buffer a single numeric value representing the acceptable movement
#' distance (m) based on the  gear type. Eventually, each gear type should have
#' a programmed value that can be overridden by the user. For now, the user must
#' specify the value in most cases. It is important to note that for fixed
#' gears, the function will check if movement > buffer, and for mobile gears the
#' function will check if movement <= buffer.

#' @returns Returns a TRUE/FALSE value with "TRUE" indicating that the gear has
#' moved in an unexpected way
#' @examples
#' GearTypeCheck(
#'      startLon=-70.92033,
#'      startLat=41.63875,
#'      endLon=-66.92022,
#'      endLat=42.16915,
#'      gearType='GNS'
#'      )
#' @export
GearTypeCheck=function(startLon,startLat,endLon,endLat,gearType,buffer=NA){
  ## Load the gear table
  devtools::load_all()
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
  ## If no buffer is provided, check to see if a default exists. If not, return
  ## an error.
  BUFFER=buffer
  if(is.na(buffer)){
    BUFFER=GEAR$BUFFER
    if(is.na(GEAR$BUFFER)){
      stop("No buffer provided and no default buffer available for this gear type. Please specify a buffer.")
    }
  }
  ## Make sure the gear is described as fixed or mobile. IF not, return an error.
  if(is.na(GEAR$TYPE)){
    stop("Gear is not classified as fixed or mobile. This test is not appropriate.")
  }
  ## Calculate the distance traveled by the gear from start to finish.
  distance=geosphere::distHaversine(
    p1=c(startLon,startLat),
    p2=c(endLon,endLat)
  )
  ## Finally, if all of the above conditions are met, run the test based on the
  ## gear type.
  result=ifelse(
    GEAR$TYPE=="FIXED",
    distance>BUFFER,
    distance<BUFFER
    )
  return(result)
}
