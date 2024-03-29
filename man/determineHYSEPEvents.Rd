% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/determineHYSEPEvents.R
\name{determineHYSEPEvents}
\alias{determineHYSEPEvents}
\title{Determine baseflow and events from HYSEP output}
\usage{
determineHYSEPEvents(HYSEPReturn, sampleDates, percent = 0.8, value = "Flow")
}
\arguments{
\item{HYSEPReturn}{dataframe returned from hysep function (in DVstats package)}

\item{sampleDates}{dataframe with two columns "Discharge_cubic_feet_per_second" and "maxSampleTime"}

\item{percent}{number to use to determine event conditions. This number will be multiplied by the flow, and if
that product is greater than the calculated baseflow, the sample time will be labeled an event.}

\item{value}{character name of discharge column.}
}
\value{
sampleDates dataframe
}
\description{
Function to find the longest continuous start and end dates from the Daily dataframe. Primary use case
is to find input value to use in a call to HYSEP (from package DVstats). If there are gaps in the data, 
the function will look for the largest continous gap.
}
\examples{
site <- "04085427"
sampleDates <- sampleDates
Start_extend <- as.character(as.Date(min(sampleDates$ActivityStartDateGiven, na.rm=TRUE))-60)
End_extend <- as.character(as.Date(max(sampleDates$ActivityStartDateGiven, na.rm=TRUE))+60)
Daily <- dataRetrieval::readNWISdv(site,'00060', Start_extend, End_extend)
Daily <- dataRetrieval::renameNWISColumns(Daily)
sampleDates <- findSampleQ(site, sampleDates, Daily)
startEnd <- getMaxStartEnd(Daily)
Start <- startEnd$Start
End <- startEnd$End
naFreeDaily <- Daily[!is.na(Daily$Flow),]
INFO <- dataRetrieval::readNWISsite(site)
DA_mi <- INFO$drain_area_va
HYSEPReturn <- exampleHYSEP
sampleDates <- determineHYSEPEvents(HYSEPReturn, sampleDates,0.8)
}
