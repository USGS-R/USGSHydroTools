#' Plot 3 baseflow/event plots from hysep output
#' 
#' Plot sliding, fixed, and local_min output of flow, with daily and instantaneous flow (when available). 
#'
#' @param sampleDates dataframe with two columns "Discharge_cubic_feet_per_second" and "maxSampleTime"
#' @param Daily dataframe from getNWISDaily function in the dataRetrieval package
#' @param INFO dataframe from getNWISSiteInfo function in dataRetrieval package. Alternatively, a dataframe with a column "station.nm"
#' @param site string USGS site identification
#' @param baseflowColumns sting vector length of 3. Names of columns with "Baseflow" or "Event" indicators.
#' @param HYSEPReturn dataframe with one column Dates, and 3 columns of baseflow as defined by HYSEPcolNames
#' @param HYSEPcolNames sting vector length of 3. Names of columns in HYSEPReturn
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
#' DA_mi <- as.numeric(INFO$drain_area_va)
#' HYSEPReturn <- exampleHYSEP
#' sampleDates <- determineHYSEPEvents(HYSEPReturn, sampleDates,0.8)
#' plotHYSEPOverview(sampleDates,Daily,INFO,site,HYSEPReturn)
plotHYSEPOverview <- function(sampleDates,Daily,INFO,site,HYSEPReturn,
                              baseflowColumns=c("flowConditionHYSEP_localMin","flowConditionHYSEP_Fixed",
                                            "flowConditionHYSEP_Sliding"),
                              HYSEPcolNames = c("LocalMin","Fixed","Sliding")){
  
  whatDischarge <- whatNWISdata(site)
  whatDischarge <-  whatDischarge[whatDischarge$parm_cd == "00060", ]  
  
  tz <- attr(sampleDates$ActivityStartDateGiven, "tzone")
  attributes(sampleDates$ActivityStartDateGiven)$tzone <- "UTC"
  tzEnd <- attr(sampleDates$ActivityEndDateGiven, "tzone")
  attributes(sampleDates$ActivityEndDateGiven)$tzone <- "UTC"
  
  Start <- as.character(as.Date(min(sampleDates$ActivityStartDateGiven, na.rm=TRUE)))
  End <- as.character(as.Date(max(sampleDates$ActivityStartDateGiven, na.rm=TRUE)))
  
  instantFlow <- NA
  
  if ("uv" %in% whatDischarge$data_type_cd){
    if(whatDischarge$begin_date[whatDischarge$data_type_cd == "uv"] < End){
      instantFlow <- readNWISuv(site,"00060",Start,End)
      instantFlow <- renameNWISColumns(instantFlow)
#       instantFlow$dateTime <- as.POSIXct(strptime(instantFlow$dateTime, format="%Y-%m-%d %H:%M:%S"), tz="UTC")
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