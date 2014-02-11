#' Plot 3 baseflow/event plots from hysep output
#' 
#' Plot sliding, fixed, and local_min output of flow, with daily and instantaneous flow (when available). 
#'
#' @param sampleDates dataframe with two columns "Discharge_cubic_feet_per_second" and "maxSampleTime"
#' @param Daily dataframe from getDVData function in the dataRetrieval package
#' @param INFO dataframe from getMetaData function in dataRetrieval package. Alternatively, a dataframe with a column "station.nm"
#' @param site string USGS site identification
#' @param baseflowColumns sting vector length of 3. Names of columns with "Baseflow" or "Event" indicators.
#' @param HYSEPReturn dataframe with one column Dates, and 3 columns of baseflow as defined by HYSEPcolNames
#' @param HYSEPcolNames sting vector length of 3. Names of columns in HYSEPReturn
#' @return sampleDates dataframe 
#' @export
#' @examples
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
#' plotHYSEPOverview(sampleDates,Daily,INFO,site,HYSEPReturn)
plotHYSEPOverview <- function(sampleDates,Daily,INFO,site,HYSEPReturn,
                              baseflowColumns=c("flowConditionHYSEP_localMin","flowConditionHYSEP_Fixed",
                                            "flowConditionHYSEP_Sliding"),
                              HYSEPcolNames = c("LocalMin","Fixed","Sliding")){
  
  whatDischarge <- getDataAvailability(site)
  whatDischarge <-  whatDischarge[whatDischarge$parameter_cd == "00060", ]  
  
  Start <- as.character(as.Date(min(sampleDates$ActivityStartDateGiven, na.rm=TRUE)))
  End <- as.character(as.Date(max(sampleDates$ActivityStartDateGiven, na.rm=TRUE)))
  
  if ("uv" %in% whatDischarge$service){
    if(whatDischarge$startDate[whatDischarge$service == "uv"] < End){
      instantFlow <- retrieveUnitNWISData(site,"00060",Start,End)
      instantFlow <- renameColumns(instantFlow)
      instantFlow$dateTime <- as.POSIXct(strptime(instantFlow$dateTime, format="%Y-%m-%d %H:%M:%S"), tz="UTC")
    }
  }
  
  par(mfrow=c(3,1),mar=c(1,4,0.5,1),oma=c(1,1,2,1)) 
  
  plotBaseflow(sampleDates,Daily,INFO,site,HYSEPReturn,
               baseflowColumns=baseflowColumns[1],
               HYSEPcolNames = HYSEPcolNames[1],plotTitle=TRUE,
               instantFlow=instantFlow,whatDischarge=whatDischarge,xlabel=FALSE)
  
  plotBaseflow(sampleDates,Daily,INFO,site,HYSEPReturn,
               baseflowColumns=baseflowColumns[2],
               HYSEPcolNames = HYSEPcolNames[2],
               instantFlow=instantFlow,whatDischarge=whatDischarge,showLegend=FALSE,
               plotTitle=FALSE,xlabel=FALSE)
  
  plotBaseflow(sampleDates,Daily,INFO,site,HYSEPReturn,
               baseflowColumns=baseflowColumns[3],
               HYSEPcolNames = HYSEPcolNames[3],
               instantFlow=instantFlow,whatDischarge=whatDischarge,showLegend=FALSE,
               plotTitle=FALSE,xlabel=TRUE)
  
}