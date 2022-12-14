% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ImpossibleDateCheck.R
\name{ImpossibleDateCheck}
\alias{ImpossibleDateCheck}
\title{ImpossibleDateCheck}
\usage{
ImpossibleDateCheck(
  dataframe,
  timecol,
  startdate = "2000-01-01",
  timezone = "UTC"
)
}
\arguments{
\item{dataframe}{a dataframe containing a column of timestamps formatted as YYYY-MM-DD hh:mm:ss}

\item{timecol}{a character string indicating which column name stores the time}

\item{startdate}{a character string indicating the earliest date of acceptable
timestamp values formatted as YYYY-MM-DD. The default is '2000-01-01'.}

\item{timezone}{a character string indicating the timezone associated with
the specified timestamp. Appropriate values can be found using the command
OlsonNames()
The default is UTC}
}
\value{
Returns a TRUE/FALSE value as to whether timestamp is out of bounds.
TRUE indicates bad data.
}
\description{
This function checks to see if the timestamp on the data is wildly out of
bounds
}
\examples{
 df=data.frame(
  LAT=c(42.16915,42.16916),
  LON=c(-66.92022,-66.92025),
  TIMESTAMP=c('2022-10-24 10:00:30','2022-10-24 10:05:30'),
  temperature=c(10,10.2),
  depth=c(100,101),
  do=c(9,109.5)
  )
ImpossibleDateCheck(
  dataframe=df,
  timecol="TIMESTAMP",
  startdate='2010-01-01',
  timezone='US/Eastern'
  )
}
