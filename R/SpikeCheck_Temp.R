## ---------------------------
## Script name: SpikeCheck_Temp
##
## Purpose of script: To check whether there is a significant difference between
## sequential temperature measurements, by comparing a measurement to the
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
#' SpikeCheck_Temp
#'
#' The SpikeCheck_Temp function checks whether there is a significant difference
#' between sequential temperature measurements, by comparing a measurement to
#' the adjacent ones. The dataframe must contain at least three lines of data.
#' @param dataframe a dataframe containing at least the following information:
#' 1) a column of timestamps in the format 'yyyy-mm-dd HH:MM:SS'
#' 2) a column of temperatures in Celsius
#' 3) a column of depths in dbars
#' @param temp_column a character string indicating the column name containing
#' the temperature values
#' @param depth_column a character string indicating the column name containing
#' the depth values
#' @param time_column a character string indicating the column name containing
#' the timestamp values
#'
#' @returns Returns the original dataframe with an additional column called
#' "TempSpike". The new column contains a TRUE/FALSE value describing
#' whether the points in the data are identified as spikes (TRUE)
#' @examples
#' df=data.frame(
#'   LAT=c(42.16915,42.16916,42.16917),
#'   LON=c(-66.92022,-66.92025,-66.92026),
#'   TIMESTAMP=c('2022-10-24 10:00:30','2022-10-24 10:05:30','2022-10-24 10:10:30'),
#'   temperature=c(10,50,10.2),
#'   depth=c(100,101,100),
#'   do=c(9,109.5,9)
#'   )
#' SpikeCheck_Temp(
#'   dataframe=df,
#'   temp_column='temperature',
#'   depth_column='depth',
#'   time_column='TIMESTAMP'
#'   )
#' @export
SpikeCheck_Temp=function(dataframe,temp_column,time_column,depth_column){
  ## Check to see whether the dataframe contains at least three rows
  if(nrow(dataframe)<3){
    stop("Dataframe too short to analyze")
  }
  ## Sort the dataframe by timestamp
  dataframe=dataframe[order(dataframe[,which(colnames(dataframe)==time_column)]),]
  ## Calculate the greatest line by line difference for each value
  dataframe$max_diff=NA
  temp_col=which(colnames(dataframe)==temp_column)
  depth_col=which(colnames(dataframe)==depth_column)
  for(i in 2:(nrow(dataframe)-1)){
    dataframe$max_diff[i]=max(
      abs(dataframe[i,temp_col]-dataframe[i-1,temp_col]),
      abs(dataframe[i,temp_col]-dataframe[i+1,temp_col])
    )
  }
  dataframe$TempSpike=ifelse(
    dataframe[,depth_col]<500,
    ifelse(
      dataframe$max_diff>6,
      TRUE,
      FALSE
      ),
    ifelse(
      dataframe$max_diff>2,
      TRUE,
      FALSE
    )
  )
  dataframe$max_diff=NULL
  return(dataframe)
}
