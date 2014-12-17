#' Summarize dataDF by group
#' 
#' Mapping routine that displays spatial dataDF variability by color differences.
#' over layers with political boundaries, hydrologic polygons, and hydrologic lines.
#' 
#' @param dataDF dataframe with columns defined by colGroup (grouping column, such as site ID), colValue (value column),
#' and optionally colDate (date columns)
#' @param colValue string defines value column in dataDF
#' @param colGroup string defines grouping column in dataDF
#' @param colDate string defines date column in dataDF. If colDate = NA, the calculations for start and end date are ignored.
#' @return dataframe with count, mean, median, min, max, start(date/time), end(date/time), and number of non-detects (nd) defined as number of NA's grouped by colGroup
#' @export
#' @examples
#' df <- data.frame(site=c("1","x","2","1","x","2"),
#'                  conc=c(2,3,4,5,NA,7),
#'                  dates=as.Date(c("2011-01-01","2011-01-01",
#'                    "2011-01-01","2011-01-02","2011-01-02","2011-01-02")))
#' sumDF <- summarizedataDF(df, "site", "conc", "dates")
summarizedataDF <- function(dataDF, colGroup, colValue, colDate){
    
  count <- aggregate(dataDF[,colGroup], by = list(dataDF[,colGroup]), FUN = length)
  names(count) <- c("group","count")
  mean <- aggregate(dataDF[,colValue], by = list(dataDF[,colGroup]), 
                    FUN = mean, na.rm=TRUE, na.action=NULL)
  names(mean) <- c("group","mean")
  median <- aggregate(dataDF[,colValue], by = list(dataDF[,colGroup]), 
                      FUN = median, na.rm=TRUE)
  names(median) <- c("group","median")
  min <- aggregate(dataDF[,colValue], by = list(dataDF[,colGroup]), 
                   FUN = function(x) ifelse(all(is.na(x)), NA, min(x, na.rm=TRUE)))
  names(min) <- c("group","min")
  max <- aggregate(dataDF[,colValue], by = list(dataDF[,colGroup]), 
                   FUN = function(x) ifelse(all(is.na(x)), NA, max(x, na.rm=TRUE)))
  names(max) <- c("group","max")
  nd <- aggregate(dataDF[,colValue], by=list(dataDF[,colGroup]),
                  FUN = function(x) sum(is.na(x)))
  names(nd) <- c("group","nd")

  summaryDF <- merge(count,mean,by = "group")
  summaryDF <- merge(summaryDF,median,by = "group")
  summaryDF <- merge(summaryDF,min,by = "group")
  summaryDF <- merge(summaryDF,max,by = "group")
  summaryDF <- merge(summaryDF,nd,by = "group")
  
  if(!is.na(colDate)){
    start <- aggregate(dataDF[,colDate], by=list(dataDF[,colGroup]),
                       FUN = min, na.rm=TRUE,na.action=NULL)
    names(start) <- c("group","start")
    end <- aggregate(dataDF[,colDate ], by=list(dataDF[,colGroup]),
                     FUN = max, na.rm=TRUE,na.action=NULL)
    names(end) <- c("group","end") 
    summaryDF <- merge(summaryDF,start,by = "group")
    summaryDF <- merge(summaryDF,end,by = "group")
  }
  
  names(summaryDF)[names(summaryDF) == "group"] <- colGroup
  
  return(summaryDF)
}