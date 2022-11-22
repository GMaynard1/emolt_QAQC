## ---------------------------
## Script name: StuckValueCheck
##
## Purpose of script: To check whether continuously repeated observations of the
## same value are produced (which may be the result of a failed sensor).
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
#' StuckValueCheck
#'
#' The StuckValueCheck function checks whether continuously repeated observations
#' of the same value are produced (which may be the result of a failed sensor)
#' by comparing a measurement to the adjacent ones. The dataframe must contain
#' at least three lines of data to run and at least five lines of data to
#' confirm that the value is stuck.
#' @param dataframe a dataframe containing at least a column of timestamps in
#' the format 'yyyy-mm-dd HH:MM:SS' along with one or more of the following
#' 1) salinity (measured in PSU)
#' 2) temperature (measured in degrees C)
#' 3) pressure (measured in dbar)
#'
#' @param salinity_column a character string indicating the column name containing
#' the salinity values. Defaults to NA.
#' @param depth_column a character string indicating the column name containing
#' the depth values. Defaults to NA.
#' @param time_column a character string indicating the column name containing
#' the timestamp values. No default value.
#' @param temp_column a character string indicating the column name containing
#' the temperature values. Defaults to NA.
#' @param tol_temp the tolerance value for temperature data in degrees C, defaults to 0.05
#' @param tol_sal the tolerance value for salinity data in PSU, defaults to 0.05
#' @param tol_press the tolerance value for pressure data in dbar, defaults to 0.5
#'
#' @returns Returns the original dataframe with an additional columns called
#' 1) StuckSalinity_Suspected
#' 2) StuckSalinity
#' 3) StuckDepth_Suspected
#' 4) StuckDepth
#' 5) StuckTemperature_Suspected
#' 6) StuckTemperature
#'
#' as appropriate, based on the user inputs. TRUE values indicate bad data.
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
#' StuckValueCheck(
#'   dataframe=df,
#'   salinity_column=NA,
#'   depth_column=NA,
#'   time_column='TIMESTAMP',
#'   temp_column=NA
#'   )
#' @export
StuckValueCheck=function(dataframe,salinity_column,depth_column,time_column,temp_column,tol_temp=0.05,tol_sal=0.05,tol_press=0.5){
  ## Sort the dataframe by timestamp
  dataframe=dataframe[order(dataframe[,which(colnames(dataframe)==time_column)]),]
  ## Work through line by line to check if values are within the tolerances
  ## specified
  dataframe$StuckSalinity_Suspected=NA
  dataframe$StuckSalinity=NA
  dataframe$StuckDepth_Suspected=NA
  dataframe$StuckDepth=NA
  dataframe$StuckTemperature_Suspected=NA
  dataframe$StuckTemperature=NA
  salinity_col=which(colnames(dataframe)==salinity_column)
  depth_col=which(colnames(dataframe)==depth_column)
  temp_col=which(colnames(dataframe)==temp_column)
  ## Check to see whether the dataframe contains at least three rows
  if(nrow(dataframe)<3){
    stop("Dataframe too short to analyze")
  } else {
    if(nrow(dataframe)<5){
      warning("Short data frame detected. Unable to confirm stuck values")
    }
    ## Check the available rows
    for(i in 2:(nrow(dataframe)-1)){
      if(is.na(salinity_column)==FALSE){
        ## Check salinity differences
        dataframe$StuckSalinity_Suspected[i]=ifelse(
          abs(dataframe[i,salinity_col]-dataframe[i-1,salinity_col])<tol_sal,
          ifelse(
            abs(dataframe[i,salinity_col]-dataframe[i+1,salinity_col])<tol_sal,
            TRUE,
            FALSE
          ),
          FALSE
        )
      }
      if(is.na(depth_column)==FALSE){
        ## Check depth differences
        dataframe$StuckDepth_Suspected[i]=ifelse(
          abs(dataframe[i,depth_col]-dataframe[i-1,depth_col])<tol_press,
          ifelse(
            abs(dataframe[i,depth_col]-dataframe[i+1,depth_col])<tol_press,
            TRUE,
            FALSE
          ),
          FALSE
        )
      }
      if(is.na(temp_column)==FALSE){
        ## Check temperature differences
        dataframe$StuckTemperature_Suspected[i]=ifelse(
          abs(dataframe[i,temp_col]-dataframe[i-1,temp_col])<tol_temp,
          ifelse(
            abs(dataframe[i,temp_col]-dataframe[i+1,temp_col])<tol_temp,
            TRUE,
            FALSE
          ),
          FALSE
        )
      }
      ## If there are enough rows, confirm stuck data
      if(i>2&(i+2)<=nrow(dataframe)){
        dataframe$StuckSalinity[i]=ifelse(
          dataframe$StuckSalinity_Suspected[i]==TRUE,
          ifelse(
            abs(dataframe[i,salinity_col]-dataframe[i-2,salinity_col])<tol_sal,
            ifelse(
              abs(dataframe[i,salinity_col]-dataframe[i+2,salinity_col])<tol_sal,
              TRUE,
              FALSE
            ),
            FALSE
          ),
          FALSE
        )
        dataframe$StuckDepth[i]=ifelse(
          dataframe$StuckDepth_Suspected[i]==TRUE,
          ifelse(
            abs(dataframe[i,depth_col]-dataframe[i-2,depth_col])<tol_press,
            ifelse(
              abs(dataframe[i,depth_col]-dataframe[i+2,depth_col])<tol_press,
              TRUE,
              FALSE
            ),
            FALSE
          ),
          FALSE
        )
        dataframe$StuckTemperature[i]=ifelse(
          dataframe$StuckTemperature_Suspected[i]==TRUE,
          ifelse(
            abs(dataframe[i,temp_col]-dataframe[i-2,temp_col])<tol_temp,
            ifelse(
              abs(dataframe[i,temp_col]-dataframe[i+2,temp_col])<tol_temp,
              TRUE,
              FALSE
            ),
            FALSE
          ),
          FALSE
        )
      }
    }
  }
  return(dataframe)
}
