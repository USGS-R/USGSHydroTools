#' Compute sine and cosine terms for dates to use as seasonal terms in data analysis
#' 
#' Function to compute seasonal sin and cosine terms from POSIXlt variable
#'
#' @param df dataframe with date included
#' @param date string column name of date to convert in POSIXlt format
#' @param return.var string prefix for variable names to return
#' @return df
#' @export
#' @examples
#' sampleData <- sampleData
#' sampleData$bpdate <- as.POSIXlt(sampleData$Hbpdate) #convert from POSIXct to POSIXlt
#' computeSeasonal(df=sampleData,date="bpdate",return.var="bdate")
computeSeasonal <- function(df,date,return.var){
  
  jday <- df[,date]$yday+1
  cos.var <- paste("cos_",return.var,sep="")
  sin.var <- paste("sin_",return.var,sep="")
  df[,cos.var] <- cos(2*pi/365*jday)
  df[,sin.var] <- sin(2*pi/365*jday)
  return(df)
}
