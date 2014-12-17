#' LoadInstantaneous
#' 
#' Computation of loadings for individual discrete samples. Results in added columns 
#' to the concentration data frame that represent the maximum flow in the 
#' original flow units, volumes in the original volume units from the flow 
#' variable, and loadings in the original mass units from the concentration
#' variable.
#' @param df.samples dataframe with discrete sample results and dates/times
#' @param Conc string column name in df.samples with the concentration results
#' @param sample.time string column name in df.samples with sample dates/times in POSIXct format
#' @param Conc2liters numeric conversion factor that converts the concentrations to a units/liter
#' @param df.Q dataframe with Q and date/time
#' @param Q string name of column in dfQ with Q
#' @param Q.time string name of column in dfQ with date/time in POSIXct format
#' @param Q2liters numeric conversion factor that converts flow to rate per liters
#' @examples
#' WQdata <- WQdata
#' flowData <- flowData
#' LoadInstantaneous(df.samples=WQdata, Conc="Total_P",
#'          sample.time="dateTime", Conc2liters=1, 
#'          df.Q=flowData, Q="Q", Q.time="pdate", Q2liters=28.3168466)
#' @export
#' @return df.load
LoadInstantaneous <- function(df.samples,Conc,sample.time,Conc2liters,df.Q,Q,Q.time,Q2liters){
  
  n <- dim(df.samples)[1]
  
  for (i in 1:n){
    
    #Determine begin and end dates and times for individual samples
    #
    if (i == 1){
      #estimate begin time for first sample to be 1/2 of the time between the
      #1st and second samples, subtracted from the 1st sample time
      bpdate <- df.samples[i,sample.time] - difftime(df.samples[(i+1),sample.time],df.samples[i,sample.time],units="secs")/2
      epdate <- mean(df.samples[,sample.time][i:(i+1)])
    } else {
      bpdate <- c(bpdate,mean(df.samples[,sample.time][(i-1):i]))
    }
    if(i==n){
      #estimate end time for last sample to be 1/2 of the time between the
      #last sample and the penultimate sample added to the last sample
      epdate <- c(epdate,df.samples[i,sample.time] + difftime(df.samples[,sample.time][i],df.samples[,sample.time][i-1],units="secs")/2)
    } else {
      if(i!=1){
      epdate <- c(epdate,mean(df.samples[,sample.time][i:(i+1)]))
      }
    }
  }

  df.samples <- cbind(df.samples,bpdate)
  df.samples <- cbind(df.samples,epdate)
  df.samples <- Hydrovol(dfQ=df.Q,Q=Q,time=Q.time,df.dates=df.samples,bdate="bpdate",edate="epdate",volume="volume",Qmax="Qmax",duration="duration")
  loads <- (df.samples[,Conc] * Conc2liters) * (df.samples$volume * Q2liters)
  
  df.samples <- cbind(df.samples,loads)
  return(df.samples)
}
