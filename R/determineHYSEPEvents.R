#' Determine baseflow and events from HYSEP output
#' 
#' Function to find the longest continuous start and end dates from the Daily dataframe. Primary use case
#' is to find input value to use in a call to HYSEP (from package DVstats). If there are gaps in the data, 
#' the function will look for the largest continous gap.
#'
#' @param HYSEPReturn dataframe returned from hysep function (in DVstats package)
#' @param sampleDates dataframe with two columns "Discharge_cubic_feet_per_second" and "maxSampleTime"
#' @param percent number to use to determine event conditions. This number will be multiplied by the flow, and if
#' that product is greater than the calculated baseflow, the sample time will be labeled an event.
#' @param value character name of discharge column.
#' @return sampleDates dataframe 
#' @export
#' @examples
#' library(dataRetrieval)
#' site <- "04085427"
#' sampleDates <- sampleDates
#' Start_extend <- as.character(as.Date(min(sampleDates$ActivityStartDateGiven, na.rm=TRUE))-60)
#' End_extend <- as.character(as.Date(max(sampleDates$ActivityStartDateGiven, na.rm=TRUE))+60)
#' Daily <- readNWISdv(site,'00060', Start_extend, End_extend)
#' Daily <- renameNWISColumns(Daily)
#' sampleDates <- findSampleQ(site, sampleDates, Daily)
#' startEnd <- getMaxStartEnd(Daily)
#' Start <- startEnd$Start
#' End <- startEnd$End
#' naFreeDaily <- Daily[!is.na(Daily$Flow),]
#' INFO <- readNWISsite(site)
#' DA_mi <- INFO$drain_area_va
#' HYSEPReturn <- exampleHYSEP
#' sampleDates <- determineHYSEPEvents(HYSEPReturn, sampleDates,0.8)
determineHYSEPEvents <- function(HYSEPReturn, sampleDates,percent=0.8, value="Flow"){
  
  sampleDates$Dates <- as.Date(sampleDates$maxSampleTime)
  sampleDates <- mergeNearest(sampleDates, "Dates",
                              right=HYSEPReturn, dates.right="Dates", max.diff="1 days")
  
  if(paste(value, "left", sep=".") %in% names(sampleDates)){
    names(sampleDates)[paste(value, "left", sep=".") == names(sampleDates)] <- value
  }
  
  sampleDates$flowConditionHYSEP_localMin <- ifelse(percent*sampleDates[,value] > sampleDates$LocalMin, "Event", "Baseflow")
  sampleDates$flowConditionHYSEP_Sliding <- ifelse(percent*sampleDates[,value] > sampleDates$Sliding, "Event", "Baseflow")
  sampleDates$flowConditionHYSEP_Fixed <- ifelse(percent*sampleDates[,value] > sampleDates$Fixed, "Event", "Baseflow")
  
  sampleDates$Dates.left <- NULL
  sampleDates$Dates.right <- NULL
  
  return(sampleDates)  
  
}