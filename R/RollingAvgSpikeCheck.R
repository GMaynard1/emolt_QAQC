## ---------------------------
## Script name: RollingAvgSpikeCheck
##
## Purpose of script: To check whether individual observations are substantially
##  different from those around them by using rolling averages.
##
## Date Created: 2023-01-06
##
## Software code created by U.S. Government employees is
## not subject to copyright in the United States
## (17 U.S.C. §105).
##
## Email: george.maynard@noaa.gov
##
## ---------------------------
## Notes: Based on the emolt_pd_cfrf_2.py script written by JiM Manning for the
##  Commercial Fisheries Research Foundation. Original code available at
##  https://github.com/jamespatrickmanning/emolt/blob/master/cfrf/emolt_pd_cfrf_2.py
##
##
## ---------------------------
#' RollingAvgSpikeCheck
#'
#' The RollingAvgSpikeCheck function checks whether each observation exceeds or
#' is lower than the rolling average by some number of standard deviations
#' (default threshold = 1 SD).
#' @param dataframe a dataframe containing timestamps and observations
#' @param column a character string indicating the column name to be checked
#' for spikes
#' @param time_column a character string indicating the column name containing
#' the timestamp values. Timestamps should be in yyyy-mm-dd hh:mm:ss format. No default value.
#' @param rolling_avg an integer indicating the number of points to include in
#' the rolling average (defaults to 30)
#' @param threshold a number indicating the standard deviation threshold to
#' trigger a QAQC flag (defaults to 1)
#' @export
#' @returns Returns the original dataframe with an additional column called
#' "FailRASpikeCheck". The new column contains a TRUE/FALSE value describing
#' whether the points fail the rate check (bad data = TRUE).
#' @examples
#' RollingAvgSpikeCheck(
#'      column="TEMPERATURE",
#'      threshold=3,
#'      dataframe=df,
#'      time_column='TIMESTAMP',
#'      )
RollingAvgSpikeCheck=function(dataframe,column,time_column,rolling_avg=30,threshold=1){
  ## Select the column of interest
  data_col=which(colnames(dataframe)==column)
  ## Create vectors to store rolling averages, standard deviations, and differences
  RA=rep(0,nrow(dataframe))
  RS=rep(0,nrow(dataframe))
  RC=rep(0,nrow(dataframe))
  dataframe$FailRASpikeCheck=NA
  ## Order the data by time
  dataframe[,which(colnames(dataframe)==time_column)]=lubridate::ymd_hms(dataframe[,which(colnames(dataframe)==time_column)])
  dataframe=dataframe[order(dataframe[,which(colnames(dataframe)==time_column)]),]
  ## Calculate the rate of change (in SD) between each measurement
  for(i in 1:(nrow(dataframe)-rolling_avg)){
    RA[i]=mean(dataframe[i:(i+rolling_avg-1),data_col],na.rm=TRUE)
    RS[i]=sd(dataframe[i:(i+rolling_avg-1),data_col],na.rm=TRUE)
    RC[i]=abs(dataframe[i,data_col]-dataframe[i+1,data_col])
    dataframe$FailRASpikeCheck[i]=RC[i]>(RS[i]*threshold)
  }
  ## Return the flagged dataset
  return(dataframe)
}
