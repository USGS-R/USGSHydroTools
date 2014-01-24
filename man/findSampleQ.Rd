\name{findSampleQ}
\alias{findSampleQ}
\title{Find flow for sample times}
\usage{
findSampleQ(site, sampleDates, localDaily)
}
\arguments{
  \item{site}{string USGS identification number}

  \item{sampleDates}{dataframe with two columns
  "ActivityStartDateGiven" and "ActivityEndDateGiven"}

  \item{localDaily}{dataframe returned from dataRetrieval}
}
\value{
sampleDates
}
\description{
Function to find flows values for given sample times. If
instantaneous data is available, this function will
retrieve that data, otherwise the Daily streamflow data
will be used. If the sample times have a start and end
time, the flow is the maximum flow in the range of the
sample.
}
\examples{
site <- "04085427"
sampleDates <- sampleDates
Start_extend <- as.character(as.Date(min(sampleDates$ActivityStartDateGiven, na.rm=TRUE))-60)
End_extend <- as.character(as.Date(max(sampleDates$ActivityStartDateGiven, na.rm=TRUE))+60)
Daily <- getDVData(site,'00060', Start_extend, End_extend,convert=FALSE)
sampleDates <- findSampleQ(site, sampleDates, Daily)
}

