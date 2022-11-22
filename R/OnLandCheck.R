## ---------------------------
## Script name: OnLandCheck.R
##
## Purpose of script: To check whether a data point is reported from land
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
#' OnLandCheck
#'
#' This function checks to see if the data fall within areas defined as land or
#' water based on the high resolution GeoJSON available from the World Bank at
#' https://datacatalog.worldbank.org/search/dataset/0038272
#' @param dataframe a dataframe containing at minimum a column for latitude and a
#' column for longitude. These columns should be named LATITUDE and LONGITUDE. There is no
#' default value.
#' @returns Returns the original dataframe with an additional column called
#' "OnLand". The new column contains a TRUE/FALSE value describing whether the
#' points in the data are on land or not. TRUE values indicate data are on land.
#' @examples
#' df=data.frame(
#'   LATITUDE=42.16915,
#'   LONGITUDE=-66.92022,
#'   temperature=10,
#'   depth=100
#'   )
#'
#' OnLandCheck(data=df)
#' @export
OnLandCheck=function(data){
  ## Standardize column names
  x=data
  colnames(x)=toupper(colnames(x))
  ## Convert to a spatial data object using the coordinates given
  sp::coordinates(x)=c('LONGITUDE','LATITUDE')
  as(x,"SpatialPoints")
  sp::proj4string(x)=sp::CRS("+proj=longlat +datum=WGS84")

  ## Check whether the data points are over land or not. An NA value indicates
  ## water.
  results=sp::over(x,land)$featurecla

  ## Convert to TRUE/FALSE values
  results=ifelse(
    is.na(results),
    FALSE,
    TRUE
  )

  ## Add the results column to the data frame and return it
  data$OnLand=results
  return(data)
}
