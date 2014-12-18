#' Find flow for sample times
#' 
#' Function to find flows values for given sample times. If instantaneous data is available, this function will 
#' retrieve that data, otherwise the Daily streamflow data will be used. If the sample times have a start
#' and end time, the flow is the maximum flow in the range of the sample. 
#'
#' @param site string USGS identification number
#' @param sampleDates dataframe with two columns "ActivityStartDateGiven" and "ActivityEndDateGiven"
#' @param localDaily dataframe returned from dataRetrieval
#' @param value character name of discharge column
#' @return sampleDates
#' @import dataRetrieval
#' @importFrom smwrBase mergeNearest
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
findSampleQ <- function(site, sampleDates,localDaily,value="Flow"){
  whatDischarge <- whatNWISdata(site)
  whatDischarge <-  whatDischarge[whatDischarge$parm_cd == "00060", ]  
  
  Start <- as.character(as.Date(min(sampleDates$ActivityStartDateGiven, na.rm=TRUE)))
  End <- as.character(as.Date(max(sampleDates$ActivityStartDateGiven, na.rm=TRUE)))

  tz <- attr(sampleDates$ActivityStartDateGiven, "tzone")
  attributes(sampleDates$ActivityStartDateGiven)$tzone <- "UTC"
  tzEnd <- attr(sampleDates$ActivityEndDateGiven, "tzone")
  attributes(sampleDates$ActivityEndDateGiven)$tzone <- "UTC"

  if ("uv" %in% whatDischarge$data_type_cd){
    instantFlow <- readNWISuv(site,"00060",Start,End)
    instantFlow <- renameNWISColumns(instantFlow)
    
    sampleDates <- mergeNearest(sampleDates, "ActivityStartDateGiven",all.left=TRUE,
                                right=instantFlow, dates.right="dateTime",max.diff="3 hours")
    row.names(sampleDates) <- NULL

    ivGap <- sampleDates[is.na(sampleDates[,"Flow_Inst"]),]
    ivGap$Date <- as.Date(ivGap$ActivityStartDateGiven)
    
    if(nrow(ivGap) > 0){
      ivGap <- mergeNearest(ivGap, "Date", all.left=TRUE,
                            right=localDaily, dates.right="Date",max.diff="3 hours")
      ivGapIndex <- which(is.na(sampleDates[,"Flow_Inst"]))
      sampleDates[ivGapIndex,value] <- ivGap$Flow_Inst
      sampleDates[ivGapIndex,paste(value,"cd",sep="_")] <- rep("Daily",nrow(ivGap))    
    }
  } else {
    #Not tested:
    sampleDates$Date <- as.Date(sampleDates$ActivityStartDateGiven)
    sampleDates <- mergeNearest(sampleDates, "Date", all.left=TRUE,
                                right=localDaily, dates.right="Date",max.diff="3 hours")
#     sampleDates$Date <- as.Date(sampleDates$Date.left)
    sampleDates[,value] <- sampleDates$Q
    sampleDates[,paste(value,"cd",sep="_")] <- sampleDates$Qualifier
  }
  
  sampleDates$maxSampleTime <- sampleDates$ActivityStartDateGiven
  if(any(!is.na(sampleDates$ActivityEndDateGiven))){
    for (k in 1:nrow(sampleDates)){
      if (!is.na(sampleDates$ActivityEndDateGiven[k])) {    
        if ("uv" %in% whatDischarge$data_type_cd){
          subFlow <- instantFlow[(instantFlow$dateTime >= sampleDates$ActivityStartDateGiven[k] & instantFlow$dateTime <= sampleDates$ActivityEndDateGiven[k]),] 
          subFlow[,value] <- subFlow$Flow_Inst
        } else {
          subFlow <- localDaily[localDaily$Date >= as.Date(sampleDates$ActivityStartDateGiven[k]) & localDaily$Date <= as.Date(sampleDates$ActivityEndDateGiven[k]),] 
          subFlow$dateTime <- as.POSIXct(localDaily$Date[localDaily$Date >= as.Date(sampleDates$ActivityStartDateGiven[k]) & localDaily$Date <= as.Date(sampleDates$ActivityEndDateGiven[k])] )
        }
        
        if(nrow(subFlow) == 0){
          subFlow <- localDaily[localDaily$Date >= as.Date(sampleDates$ActivityStartDateGiven[k]) & localDaily$Date <= as.Date(sampleDates$ActivityEndDateGiven[k]),] 
          subFlow$dateTime <- as.POSIXct(localDaily$Date[localDaily$Date >= as.Date(sampleDates$ActivityStartDateGiven[k]) & localDaily$Date <= as.Date(sampleDates$ActivityEndDateGiven[k])] )
        }
        
        maxFlow <- max(subFlow[,value], na.rm=TRUE)
        
        if(is.finite(maxFlow)){
          sampleDates$maxSampleTime[k] <- as.POSIXct(mean(subFlow$dateTime[subFlow[,value] == maxFlow],na.rm=TRUE), tz="UTC")
          sampleDates[k,value] <- maxFlow
        }
      }        
    }    
  }

  attributes(sampleDates$ActivityStartDateGiven)$tzone <- tz
  attributes(sampleDates$ActivityEndDateGiven)$tzone <- tzEnd
  
  sampleDates <- sampleDates[c("ActivityStartDateGiven", "ActivityEndDateGiven", 
                               value, paste(value,"cd",sep="_"), 
                               "maxSampleTime")]
  
  return(sampleDates)
  
}