## ---------------------------
## Script name: GRC_oxygen.R
##
## Purpose of script: To check whether the reported value is within the range of
##    the sensor used.
##
## Date Created: 2022-10-24
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
#' GRC_oxygen
#'
#' This function checks whether the reported oxygen value is within the range of
#'    the sensor used.
#' @param dataframe a dataframe containing a column of dissolved oxygen values
#' reported in mg/L
#' @param column a character string indicating the column name containing the
#' oxygen values
#' @param make The make of the sensor used to collect the data. Acceptable
#' options can be found using ListSensors()
#' @param model The model of the sensor used to collect the data. Acceptable
#' options can be found using ListSensors()
#'
#' @returns Returns the original dataframe with an additional column called
#' "InvalidOxygen". The new column contains a TRUE/FALSE value describing
#' whether the points in the data occur outside of the sensor's limits (TRUE)
#' @examples
#' df=data.frame(
#'   LAT=c(42.16915,42.16916),
#'   LON=c(-66.92022,-66.92025),
#'   TIMESTAMP=c('2022-10-24 10:00:30','2022-10-24 10:05:30'),
#'   temperature=c(10,10.2),
#'   depth=c(100,101),
#'   do=c(9,9.5)
#'   )
#' GRC_oxygen(
#'   dataframe=df,
#'   column='do',
#'   make='Lowell',
#'   model='DOT-2'
#'   )
#' @export
GRC_oxygen=function(dataframe,column,make,model){
  ## Standardize make and model and retrieve the limits from the built-in
  ## sensors dataset
  sensor=which(sensors$Make==toupper(make)&sensors$Model==toupper(model))
  if(length(sensor)==0){
    stop("Sensor not found. Please use ListSensors()")
  }
  s_min=sensors$MinDo[sensor]
  s_max=sensors$MaxDo[sensor]
  if(is.na(s_min)|is.na(s_max)){
    stop("Sensor limits for this parameter not established.")
  }
  colnum=which(colnames(dataframe)==column)
  dataframe$InvalidOxygen=ifelse(
    dataframe[,colnum]>=s_min&dataframe[,colnum]<=s_max,
    FALSE,
    TRUE
  )
  return(dataframe)
}
