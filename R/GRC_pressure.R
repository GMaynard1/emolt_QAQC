## ---------------------------
## Script name: GRC_pressure.R
##
## Purpose of script: To check whether the reported value is within the range of
##    the sensor used.
##
## Date Created: 2022-10-25
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
#' GRC_pressure
#'
#' This function checks whether the reported pressure value is within the range
#' of the sensor used.
#' @param dataframe a dataframe containing a column of pressure values reported
#' in decibars
#' @param column a character string indicating the column name containing the
#' pressure values
#' @param make The make of the sensor used to collect the data. Acceptable
#' options can be found using ListSensors()
#' @param model The model of the sensor used to collect the data. Acceptable
#' options can be found using ListSensors()
#'
#' @returns Returns the original dataframe with an additional column called
#' "InvalidPressure". The new column contains a TRUE/FALSE value describing
#' whether the points in the data occur outside of the sensor's limits (TRUE)
#' @examples
#' df=data.frame(
#'   LAT=c(42.16915,42.16916),
#'   LON=c(-66.92022,-66.92025),
#'   TIMESTAMP=c('2022-10-24 10:00:30','2022-10-24 10:05:30'),
#'   temperature=c(10,10.2),
#'   depth=c(199,202),
#'   do=c(9,9.5)
#'   )
#' GRC_pressure(
#'   dataframe=df,
#'   column='depth',
#'   make='ZebraTech',
#'   model='Moana-200'
#'   )
GRC_pressure=function(dataframe,column,make,model){
  ## Standardize make and model and retrieve the limits from the built-in
  ## sensors dataset
  sensor=which(sensors$Make==toupper(make)&sensors$Model==toupper(model))
  if(length(sensor)==0){
    stop("Sensor not found. Please use ListSensors()")
  }
  s_min=sensors$MinPress[sensor]
  s_max=sensors$MaxPress[sensor]
  if(is.na(s_min)|is.na(s_max)){
    stop("Sensor limits for this parameter not established.")
  }
  colnum=which(colnames(dataframe)==column)
  dataframe$InvalidPressure=ifelse(
    dataframe[,colnum]>=s_min&dataframe[,colnum]<=s_max,
    FALSE,
    TRUE
  )
  return(dataframe)
}
