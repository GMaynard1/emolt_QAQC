## ---------------------------
## Script name: ListSensors.R
##
## Purpose of script: To list the make and model of any sensors included in the
## package data
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
#' ListSensors
#' @param make filter only sensors of this make. Defaults to NA.
#' @param model filter only sensors of this model. Defaults to NA.
#'
#' @returns a vector of possible Make/Model combinations for use in this package
#' @examples
#' ListSensors()
#' @export
ListSensors=function(make=NA,model=NA){
  sensors=sensors[order(sensors$Model),]
  sensors=sensors[order(sensors$Make),]
  if(is.na(make)&is.na(model)){
    return(dplyr::select(sensors,Make,Model))
  }
  if(is.na(make)==FALSE){
    x=subset(sensors,sensors$Make==toupper(make))
    if(is.na(model)==FALSE){
      x=subset(x,x$Model==toupper(model))
      return(dplyr::select(x,Make,Model))
    } else {
      return(dplyr::select(x,Make,Model))
    }
  }
  if(is.na(model)==FALSE){
    x=subset(sensors,sensors$Model==toupper(model))
    return(dplyr::select(x,Make,Model))
  }
}
