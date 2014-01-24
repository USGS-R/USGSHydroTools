#' Find flow for sample times
#' 
#' Function to find flows values for given sample times. If instantaneous data is available, this function will 
#' retrieve that data, otherwise the Daily streamflow data will be used. If the sample times have a start
#' and end time, the flow is the maximum flow in the range of the sample. 
#'
#' @param site string USGS identification number
#' @param sampleDates dataframe with two columns "ActivityStartDateGiven" and "ActivityEndDateGiven"
#' @param localDaily dataframe returned from dataRetrieval
#' @return sampleDates
#' @import dataRetrieval
#' @import USGSwsBase
#' @export
#' @examples
#' site <- "04085427"
#' sampleDates <- sampleDates
#' Start_extend <- as.character(as.Date(min(sampleDates$ActivityStartDateGiven, na.rm=TRUE))-60)
#' End_extend <- as.character(as.Date(max(sampleDates$ActivityStartDateGiven, na.rm=TRUE))+60)
#' Daily <- getDVData(site,'00060', Start_extend, End_extend,convert=FALSE)
#' sampleDates <- findSampleQ(site, sampleDates, Daily)
findSampleQ <- function(site, sampleDates,localDaily){
  whatDischarge <- getDataAvailability(site)
  whatDischarge <-  whatDischarge[whatDischarge$parameter_cd == "00060", ]  
  
  Start <- as.character(as.Date(min(sampleDates$ActivityStartDateGiven, na.rm=TRUE)))
  End <- as.character(as.Date(max(sampleDates$ActivityStartDateGiven, na.rm=TRUE)))
  
  if ("uv" %in% whatDischarge$service){
    instantFlow <- retrieveUnitNWISData(site,"00060",Start,End)
    instantFlow <- renameColumns(instantFlow)
    instantFlow$dateTime <- as.POSIXct(strptime(instantFlow$dateTime, format="%Y-%m-%d %H:%M:%S"), tz="UTC")
    
    sampleDates <- mergeNearest(sampleDates, "ActivityStartDateGiven",all.left=TRUE,
                                right=instantFlow, dates.right="dateTime",max.diff="3 hours")
    row.names(sampleDates) <- NULL
    sampleDates$Discharge_cubic_feet_per_second_cd <- as.character(sampleDates$Discharge_cubic_feet_per_second_cd)
    ivGap <- sampleDates[is.na(sampleDates$Discharge_cubic_feet_per_second),]
    ivGap$Date <- as.Date(ivGap$ActivityStartDateGiven)
    
    if(nrow(ivGap) > 0){
      ivGap <- mergeNearest(ivGap, "Date", all.left=TRUE,
                            right=localDaily, dates.right="Date",max.diff="3 hours")
      ivGapIndex <- which(is.na(sampleDates$Discharge_cubic_feet_per_second))
      sampleDates$Discharge_cubic_feet_per_second[ivGapIndex] <- ivGap$Q
      sampleDates$Discharge_cubic_feet_per_second_cd[ivGapIndex] <- rep("Daily",nrow(ivGap))    
    }
  } else {
    #Not tested:
    sampleDates$Date <- as.Date(sampleDates$ActivityStartDateGiven)
    sampleDates <- mergeNearest(sampleDates, "Date", all.left=TRUE,
                                right=localDaily, dates.right="Date",max.diff="3 hours")
#     sampleDates$Date <- as.Date(sampleDates$Date.left)
    sampleDates$Discharge_cubic_feet_per_second <- sampleDates$Q
    sampleDates$Discharge_cubic_feet_per_second_cd <- sampleDates$Qualifier
  }
  
  sampleDates$maxSampleTime <- sampleDates$ActivityStartDateGiven
  if(any(!is.na(sampleDates$ActivityEndDateGiven))){
    for (k in 1:nrow(sampleDates)){
      if (!is.na(sampleDates$ActivityEndDateGiven[k])) {          
        if ("uv" %in% whatDischarge$service){
          subFlow <- instantFlow[(instantFlow$dateTime >= sampleDates$ActivityStartDateGiven[k] & instantFlow$dateTime <= sampleDates$ActivityEndDateGiven[k]),] 
        } else {
          subFlow <- localDaily[localDaily$Date >= as.Date(sampleDates$ActivityStartDateGiven[k]) & localDaily$Date <= as.Date(sampleDates$ActivityEndDateGiven[k]),] 
          subFlow$Discharge_cubic_feet_per_second <- subFlow$Q
          subFlow$dateTime <- as.POSIXct(localDaily$Date[localDaily$Date >= as.Date(sampleDates$ActivityStartDateGiven[k]) & localDaily$Date <= as.Date(sampleDates$ActivityEndDateGiven[k])] )
        }
        
        if(nrow(subFlow) == 0){
          subFlow <- localDaily[localDaily$Date >= as.Date(sampleDates$ActivityStartDateGiven[k]) & localDaily$Date <= as.Date(sampleDates$ActivityEndDateGiven[k]),] 
          subFlow$Discharge_cubic_feet_per_second <- subFlow$Q
          subFlow$dateTime <- as.POSIXct(localDaily$Date[localDaily$Date >= as.Date(sampleDates$ActivityStartDateGiven[k]) & localDaily$Date <= as.Date(sampleDates$ActivityEndDateGiven[k])] )
        }
        
        maxFlow <- max(subFlow$Discharge_cubic_feet_per_second, na.rm=TRUE)
        
        if(is.finite(maxFlow)){
          sampleDates$maxSampleTime[k] <- as.POSIXct(mean(subFlow$dateTime[subFlow$Discharge_cubic_feet_per_second == maxFlow],na.rm=TRUE), tz="UTC")
          sampleDates$Discharge_cubic_feet_per_second[k] <- maxFlow
        }
      }        
    }    
  }
  
  sampleDates <- sampleDates[c("ActivityStartDateGiven", "ActivityEndDateGiven", 
                               "Discharge_cubic_feet_per_second", "Discharge_cubic_feet_per_second_cd", 
                               "maxSampleTime")]
  
  return(sampleDates)
  
}