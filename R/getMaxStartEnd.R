#' Find maximum start and end dates from Daily dataframe
#' 
#' Function to find the longest continuous start and end dates from the Daily dataframe. Primary use case
#' is to find input value to use in a call to HYSEP (from package DVStats). If there are gaps in the data, 
#' the function will look for the largest continous gap.
#'
#' @param localDaily dataframe returned from dataRetrieval
#' @param value character name of discharge column
#' @param date character name of date column
#' @return named list with Start and End values
#' @export
#' @examples
#' library(dataRetrieval)
#' site <- "04085427"
#' sampleDates <- sampleDates
#' Start_extend <- as.character(as.Date(min(sampleDates$ActivityStartDateGiven, na.rm=TRUE))-60)
#' End_extend <- as.character(as.Date(max(sampleDates$ActivityStartDateGiven, na.rm=TRUE))+60)
#' Daily <- readNWISdv(site,'00060', Start_extend, End_extend)
#' Daily <- renameNWISColumns(Daily)
#' startEnd <- getMaxStartEnd(Daily)
#' Start <- startEnd$Start
#' End <- startEnd$End
getMaxStartEnd <- function(localDaily, value="Flow", date="Date"){
  naFreeDaily <- localDaily[!is.na(localDaily[,value]),]
  dataPoints <- nrow(naFreeDaily)
  naFreeDaily$Julian <- as.numeric(julian(localDaily[,date],origin=as.Date("1850-01-01")))
  difference <- (naFreeDaily$Julian[dataPoints] - naFreeDaily$Julian[1])+1  
  numberOfGaps <- 0
  if (dataPoints != difference){
    for(j in 2:dataPoints) {
      if((naFreeDaily$Julian[j]-naFreeDaily$Julian[j-1])>1){
        startGap <- naFreeDaily$Date[j-1]
        startGapIndex <- j-1
        stopGap <- naFreeDaily$Date[j]
        stopGapIndex <- j
        message("Gap from", as.character(startGap), "to", as.character(stopGap),"\n")
        numberOfGaps <- 1 + numberOfGaps
      }
    }
  } else {
    Start <- naFreeDaily[1,date]
    End <- naFreeDaily[dataPoints,date]
    
  }
  
  if (numberOfGaps == 1){
    maxRange <- which.max(c(startGapIndex, dataPoints-stopGapIndex))
    if (maxRange == 1){
      Start <- naFreeDaily[1,date]
      End <- naFreeDaily[startGapIndex,date]
    } else {
      Start <- naFreeDaily[stopGapIndex,date]
      End <- naFreeDaily[dataPoints,date]
    }
  }
  
  if (numberOfGaps > 1){
    for(j in 2:dataPoints) {
      if((naFreeDaily$Julian[j]-naFreeDaily$Julian[j-1])>1){
        startGap <- naFreeDaily[j-1,date]
        startGapIndex <- j-1
        Start <- naFreeDaily[1,date]
        End <- naFreeDaily[startGapIndex,date]
        break
      }
    }      
  }
  return(list(Start=Start,End=End))
  
}