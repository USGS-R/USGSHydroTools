#' Plot baseflow/event plot
#' 
#' Plot output of flow, with daily and instantaneous flow (when available). 
#'
#' @param sampleDates dataframe with two columns "Discharge_cubic_feet_per_second" and "maxSampleTime"
#' @param Daily dataframe from getDVData function in the dataRetrieval package
#' @param INFO dataframe from getMetaData function in dataRetrieval package. Alternatively, a dataframe with a column "station.nm"
#' @param site string USGS site identification
#' @param baseflowColumns string. Names of columns in the sampleDates dataframe with "Baseflow" or "Event" indicators.
#' @param HYSEPReturn dataframe with one column Dates, and at least 1 column of baseflow
#' @param HYSEPcolNames string. Name of column in HYSEPReturn.
#' @param xlabel logical. Whether or not to print x label
#' @param showLegend logical. Whether or not to print legend
#' @param plotTitle logical. Whether or not to print title
#' @param instantFlow dataframe returned from retrieveUnitNWISData. If none available, NA.
#' @param whatDischarge dataframe returned from getDataAvailability
#' @export
#' @examples
#' library(dataRetrieval)
#' site <- "04085427"
#' sampleDates <- sampleDates
#' Start_extend <- as.character(as.Date(min(sampleDates$ActivityStartDateGiven, na.rm=TRUE))-60)
#' End_extend <- as.character(as.Date(max(sampleDates$ActivityStartDateGiven, na.rm=TRUE))+60)
#' Daily <- getDVData(site,'00060', Start_extend, End_extend,convert=FALSE)
#' sampleDates <- findSampleQ(site, sampleDates, Daily)
#' startEnd <- getMaxStartEnd(Daily)
#' Start <- startEnd$Start
#' End <- startEnd$End
#' naFreeDaily <- Daily[!is.na(Daily$Q),]
#' INFO <- getSiteFileData(site)
#' DA_mi <- as.numeric(INFO$drain.area.va)
#' HYSEPReturn <- exampleHYSEP
#' sampleDates <- determineHYSEPEvents(HYSEPReturn, sampleDates,0.8)
#' whatDischarge <- getDataAvailability(site)
#' whatDischarge <-  whatDischarge[whatDischarge$parameter_cd == "00060", ]
#' Start <- as.character(as.Date(min(sampleDates$ActivityStartDateGiven, na.rm=TRUE)))
#' End <- as.character(as.Date(max(sampleDates$ActivityStartDateGiven, na.rm=TRUE)))
#' 
#' if ("uv" %in% whatDischarge$service){
#'   if(whatDischarge$startDate[whatDischarge$service == "uv"] < End){
#'     instantFlow <- retrieveNWISunitData(site,"00060",Start,End)
#'     instantFlow <- renameColumns(instantFlow)
#'     instantFlow$dateTime <- as.POSIXct(strptime(instantFlow$dateTime, format="%Y-%m-%d %H:%M:%S"), tz="UTC")
#'   }
#' }
#' plotBaseflow(sampleDates,Daily,INFO,site,HYSEPReturn,
#'              baseflowColumns="flowConditionHYSEP_localMin",
#'              HYSEPcolNames = "LocalMin",plotTitle=TRUE,
#'              instantFlow=instantFlow,whatDischarge=whatDischarge,xlabel=FALSE)
plotBaseflow <- function(sampleDates,Daily,INFO,site,HYSEPReturn,
                              baseflowColumns="flowConditionHYSEP_localMin",
                              HYSEPcolNames = "LocalMin",
                              xlabel=TRUE, showLegend=TRUE,plotTitle=TRUE,
                              instantFlow=NA,whatDischarge){
    
  Start <- as.character(as.Date(min(sampleDates$ActivityStartDateGiven, na.rm=TRUE)))
  End <- as.character(as.Date(max(sampleDates$ActivityStartDateGiven, na.rm=TRUE)))
  
  mainTitle <- paste(INFO$station.nm,": ", site,sep="")
  minYear <- as.integer(strsplit(Start,"-")[[1]][1])
  maxYear <- as.integer(strsplit(End,"-")[[1]][1])+1
  
  xAxisLocs <-  seq(as.Date(paste(minYear,"/1/1",sep="")), as.Date(paste(maxYear,"/1/1",sep="")), by = "6 months")
  
  indexForX <- which(Daily$Date %in% xAxisLocs)

  xLabelText <- ifelse(xlabel, "Dates", "")
  xaxtText <- ifelse(xlabel, "s", "n")
  
  composites <- sampleDates[!is.na(sampleDates$ActivityEndDateGiven),]
  
  plot(as.POSIXct(Daily$Date), Daily$Q, type="l",ylim=c(0,max(Daily$Q,na.rm=TRUE)),
       xlab= xLabelText, ylab="Discharge[cfs]", xaxt=xaxtText, tck = 0.02)
  if ("uv" %in% whatDischarge$service){
    if(whatDischarge$startDate[whatDischarge$service == "uv"] < End){
      lines(instantFlow$dateTime, instantFlow$Discharge_cubic_feet_per_second, col="azure4")
    }
  } 
  polygon(as.POSIXct(c(HYSEPReturn$Dates[1], HYSEPReturn$Dates,HYSEPReturn$Dates[length(HYSEPReturn$Dates)])), c(0,HYSEPReturn[[HYSEPcolNames[1]]],0), col="beige")
  
  points(sampleDates$maxSampleTime[sampleDates[baseflowColumns] == "Event"], 
         sampleDates$Discharge_cubic_feet_per_second[sampleDates[baseflowColumns[1]] == "Event"],
         pch=16,col="red",cex=1)
  points(sampleDates$maxSampleTime[sampleDates[baseflowColumns] == "Baseflow"], 
         sampleDates$Discharge_cubic_feet_per_second[sampleDates[baseflowColumns[1]] == "Baseflow"],
         pch=16,col="blue",cex=1)
  text(grconvertX(0.9, from = "npc", to = "user"), grconvertY(0.9, from = "npc", to = "user"), HYSEPcolNames[1])
  axis(1,labels=FALSE, at=as.POSIXct(Daily$Date)[indexForX],tck = 0.02)
  axis(3,labels=FALSE, at=as.POSIXct(Daily$Date)[indexForX], tck = 0.02)
  axis(4,labels=FALSE, tck = 0.02)
  
  if(plotTitle){
    title(mainTitle,outer=TRUE)
  }
  
  if(showLegend){
    legend(grconvertX(0.01, from = "npc", to = "user"), grconvertY(0.95, from = "npc", to = "user"), 
           c("Flow (Instantaneous)", "Flow (Daily)","Base","Baseflow Sample","Event Sample"),
           fill=c(NA,NA,"beige",NA,NA),
           pch=c(NA,NA,NA,16,16),
           border=c(NA,NA,"black",NA,NA),
           lty=c(1,1,NA,NA,NA),
           col=c("azure4","black",NA,"blue","red"))
  }
  
  if(nrow(composites) > 0){
    segments(composites$ActivityStartDateGiven, composites$Discharge_cubic_feet_per_second, 
             composites$ActivityEndDateGiven, composites$Discharge_cubic_feet_per_second)
    delta = 0.01*(par('usr')[4] - par('usr')[3])
    segments(composites$ActivityStartDateGiven, composites$Discharge_cubic_feet_per_second+delta, 
             composites$ActivityStartDateGiven, composites$Discharge_cubic_feet_per_second-delta)
    segments(composites$ActivityEndDateGiven, composites$Discharge_cubic_feet_per_second+delta, 
             composites$ActivityEndDateGiven, composites$Discharge_cubic_feet_per_second-delta)
  }

  
}