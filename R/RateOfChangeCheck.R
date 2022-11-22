## ---------------------------
## Script name: RateOfChangeCheck.R
##
## Purpose of script: This test is applied per segment (Up-Down-Bottom), and
##   inspects the segments on a rate of change exceeding a threshold defined by
##   the operator. In this case the thresholds are based on the IOOS examples
##   (U.S. Integrated Ocean Observing System, 2020), where the rate of change
##   between measurement Tn-1 and Tn must be less than three standard deviations
##   (3*SD). The SD of the T time series is computed over the full segment.
##
## Date Created: 2022-11-03
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
#' RateofChangeCheck
#'
#' This test is applied per segment (Up-Down-Bottom), and
#'   inspects the segments on a rate of change exceeding a threshold defined by
#'   the operator. In this case the thresholds are based on the IOOS examples
#'   (U.S. Integrated Ocean Observing System, 2020), where the rate of change
#'   between measurement Tn-1 and Tn must be less than three standard deviations
#'   (3*SD). The SD of the T time series is computed over the full segment.
#' @param column The column name to apply the test to. There is no default value.
#' @param threshold The threshold (in SD) of change that is considered
#' acceptable. The default value is 3.
#' @param dataframe a dataframe containing at least the column TIMESTAMP in the
#' format yyyy-mm-dd HH:MM:SS and one parameter of interest. There is no default
#' value.
#' @returns Returns the original dataframe with an additional column called
#' "FailRateCheck". The new column contains a TRUE/FALSE value describing
#' whether the points fail the rate check (bad data = TRUE).
#' @examples
#' RateofChangeCheck(
#'      column="TEMPERATURE",
#'      threshold=3,
#'      dataframe=df
#'      )
RateOfChangeCheck=function(column,threshold=3,dataframe){
  ## Select the column of interest
  data_col=which(colnames(dataframe)==column)
  ## Calculate the standard deviation for the full dataset
  SD=sd(dataframe[,data_col])
  ## Order the data by time
  dataframe=dataframe[order(dataframe$TIMESTAMP),]
  ## Create an empty vector to store the rate of change in
  ROC=rep(NA,nrow(dataframe))
  ## Calculate the rate of change (in SD) between each measurement
  for(i in 2:nrow(dataframe)){
    ROC[i]=abs(dataframe[i,data_col]-dataframe[i-1,data_col])/SD
  }
  dataframe$FailRateCheck=ROC>threshold
  return(dataframe)
}
