## ---------------------------
## Script name: UnlikelyTempCheck.R
##
## Purpose of script: To check whether the recorded date is wildly out of bounds
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
#' UnlikelyTempCheck
#'
#' This function checks to see if the temperature records are wildly out of
#' bounds
#' @param dataframe a dataframe containing a column of temperatures
#' @param temp_column a character string indicating which column name stores the time
#' @param min_temp an integer indicating the lowest acceptable temperature (defaults to 0)
#' @param max_temp an integer indicating the highest acceptable temperature (defaults to 30)
#' @export
#' @returns Returns a dataframe containing a TRUE/FALSE value as to whether the temperature is out of bounds.
#' TRUE indicates bad data.
#' @examples
#'  df=data.frame(
#'   LAT=c(42.16915,42.16916),
#'   LON=c(-66.92022,-66.92025),
#'   TIMESTAMP=c('2022-10-24 10:00:30','2022-10-24 10:05:30'),
#'   temperature=c(10,10.2),
#'   depth=c(100,101),
#'   do=c(9,109.5)
#'   )
#' UnlikelyTempCheck(
#'   dataframe=df,
#'   temp_column="temperature",
#'   )

UnlikelyTempCheck=function(dataframe,temp_column,min_temp=0,max_temp=30){
  ## Record whether the timestamp is out of bounds
  new_dat=dataframe
  new_dat$temp_oob=ifelse(
    new_dat[,which(colnames(new_dat)==temp_column)]<max_temp&
      new_dat[,which(colnames(dataframe)==temp_column)]>min_temp,
    FALSE,
    TRUE
  )
  return(new_dat)
}
