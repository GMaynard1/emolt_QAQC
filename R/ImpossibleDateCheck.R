## ---------------------------
## Script name: ImpossibleDateCheck.R
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
#' ImpossibleDateCheck
#'
#' This function checks to see if the timestamp on the data is wildly out of
#' bounds
#' @param dataframe a dataframe containing a column of timestamps formatted as YYYY-MM-DD hh:mm:ss
#' @param timecol a character string indicating which column name stores the time
#' @param startdate a character string indicating the earliest date of acceptable
#' timestamp values formatted as YYYY-MM-DD. The default is '2000-01-01'.
#' @param timezone a character string indicating the timezone associated with
#' the specified timestamp. Appropriate values can be found using the command
#' OlsonNames()
#' The default is UTC

#' @returns Returns a TRUE/FALSE value as to whether timestamp is out of bounds.
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
#' ImpossibleDateCheck(
#'   dataframe=df,
#'   timecol="TIMESTAMP",
#'   startdate='2010-01-01',
#'   timezone='US/Eastern'
#'   )
#' @export
ImpossibleDateCheck=function(dataframe,timecol,startdate='2000-01-01',timezone='UTC'){
  ## Convert the timestamp to a POSIX value
  dataframe[,which(colnames(dataframe)==timecol)]=lubridate::ymd_hms(
    dataframe[,which(colnames(dataframe)==timecol)]
    )
  ## If necessary, convert the timestamp to UTC
  if(timezone!='UTC'){
    dataframe[,which(colnames(dataframe)==timecol)]=lubridate::with_tz(
      dataframe[,which(colnames(dataframe)==timecol)],
      tzone="UTC"
      )
  }
  ## Record whether the timestamp is out of bounds
  new_dat=dataframe
  new_dat$time_oob=ifelse(
    new_dat[,which(colnames(new_dat)==timecol)]>lubridate::ymd(startdate)&
      new_dat[,which(colnames(dataframe)==timecol)]<lubridate::with_tz(Sys.time(),tzone='UTC'),
    FALSE,
    TRUE
  )
  return(new_dat)
}
