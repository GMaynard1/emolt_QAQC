## ---------------------------
## Script name: SpikeCheck_Salinity
##
## Purpose of script: To check whether there is a significant difference between
## sequential salinity measurements, by comparing a measurement to the
## adjacent ones
##
## Date Created: 2022-11-02
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
#' SpikeCheck_Salinity
#'
#' The SpikeCheck_Semp function checks whether there is a significant difference
#' between sequential salinity measurements, by comparing a measurement to
#' the adjacent ones. The dataframe must contain at least three lines of data.
#' @param dataframe a dataframe containing at least the following information:
#' 1) a column of timestamps in the format 'yyyy-mm-dd HH:MM:SS'
#' 2) a column of salinity in PSU
#' 3) a column of depths in dbars
#' @param salinity_column a character string indicating the column name containing
#' the salinity values
#' @param depth_column a character string indicating the column name containing
#' the depth values
#' @param time_column a character string indicating the column name containing
#' the timestamp values
#'
#' @returns Returns the original dataframe with an additional column called
#' "SalinitySpike". The new column contains a TRUE/FALSE value describing
#' whether the points in the data are identified as spikes (TRUE)
#' @examples
#' df=data.frame(
#'   LAT=c(42.16915,42.16916,42.16917),
#'   LON=c(-66.92022,-66.92025,-66.92026),
#'   TIMESTAMP=c('2022-10-24 10:00:30','2022-10-24 10:05:30','2022-10-24 10:10:30'),
#'   temperature=c(10,50,10.2),
#'   depth=c(100,101,100),
#'   do=c(9,109.5,9),
#'   salinity=c(35,38,35.1)
#'   )
#' SpikeCheck_Salinity(
#'   dataframe=df,
#'   salinity_column='salinity',
#'   depth_column='depth',
#'   time_column='TIMESTAMP'
#'   )
#' @export
SpikeCheck_Salinity=function(dataframe,salinity_column,depth_column,time_column){
  ## Check to see whether the dataframe contains at least three rows
  if(nrow(dataframe)<3){
    stop("Dataframe too short to analyze")
  }
  ## Sort the dataframe by timestamp
  dataframe=dataframe[order(dataframe[,which(colnames(dataframe)==time_column)]),]
  ## Calculate the greatest line by line difference for each value
  dataframe$max_diff=NA
  salinity_col=which(colnames(dataframe)==salinity_column)
  depth_col=which(colnames(dataframe)==depth_column)
  for(i in 2:(nrow(dataframe)-1)){
    dataframe$max_diff[i]=max(
      abs(dataframe[i,salinity_col]-dataframe[i-1,salinity_col]),
      abs(dataframe[i,salinity_col]-dataframe[i+1,salinity_col])
    )
  }
  dataframe$salinitySpike=ifelse(
    dataframe[,depth_col]<500,
    ifelse(
      dataframe$max_diff>0.9,
      TRUE,
      FALSE
    ),
    ifelse(
      dataframe$max_diff>0.3,
      TRUE,
      FALSE
    )
  )
  dataframe$max_diff=NULL
  return(dataframe)
}
