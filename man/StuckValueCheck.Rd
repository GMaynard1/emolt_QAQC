% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StuckCheck.R
\name{StuckValueCheck}
\alias{StuckValueCheck}
\title{StuckValueCheck}
\usage{
StuckValueCheck(
  dataframe,
  salinity_column,
  depth_column,
  time_column,
  temp_column,
  tol_temp = 0.05,
  tol_sal = 0.05,
  tol_press = 0.5
)
}
\arguments{
\item{dataframe}{a dataframe containing at least a column of timestamps in
the format 'yyyy-mm-dd HH:MM:SS' along with one or more of the following
\enumerate{
\item salinity (measured in PSU)
\item temperature (measured in degrees C)
\item pressure (measured in dbar)
}}

\item{salinity_column}{a character string indicating the column name containing
the salinity values. Defaults to NA.}

\item{depth_column}{a character string indicating the column name containing
the depth values. Defaults to NA.}

\item{time_column}{a character string indicating the column name containing
the timestamp values. No default value.}

\item{temp_column}{a character string indicating the column name containing
the temperature values. Defaults to NA.}

\item{tol_temp}{the tolerance value for temperature data in degrees C, defaults to 0.05}

\item{tol_sal}{the tolerance value for salinity data in PSU, defaults to 0.05}

\item{tol_press}{the tolerance value for pressure data in dbar, defaults to 0.5}
}
\value{
Returns the original dataframe with an additional columns called
\enumerate{
\item StuckSalinity_Suspected
\item StuckSalinity
\item StuckDepth_Suspected
\item StuckDepth
\item StuckTemperature_Suspected
\item StuckTemperature
}

as appropriate, based on the user inputs. TRUE values indicate bad data.
}
\description{
The StuckValueCheck function checks whether continuously repeated observations
of the same value are produced (which may be the result of a failed sensor)
by comparing a measurement to the adjacent ones. The dataframe must contain
at least three lines of data to run and at least five lines of data to
confirm that the value is stuck.
}
\examples{
df=data.frame(
  LAT=c(42.16915,42.16916,42.16917),
  LON=c(-66.92022,-66.92025,-66.92026),
  TIMESTAMP=c('2022-10-24 10:00:30','2022-10-24 10:05:30','2022-10-24 10:10:30'),
  temperature=c(10,50,10.2),
  depth=c(100,101,100),
  do=c(9,109.5,9),
  salinity=c(35,38,35.1)
  )
StuckValueCheck(
  dataframe=df,
  salinity_column=NA,
  depth_column=NA,
  time_column='TIMESTAMP',
  temp_column=NA
  )
}
