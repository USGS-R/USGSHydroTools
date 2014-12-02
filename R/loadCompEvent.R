#' LoadCompEvent
#' 
#' Computation of loadings for event periods using individual discrete samples. 
#' Results in added columns to the event data frame that represent the 
#' event loadings in the original mass units from the concentration variable 
#' and the flow-weighted event mean concentration.
#' maximum flow in the original flow units, volumes in the original volume units 
#' from the flow variable, and loadings in the original mass units from the 
#' concentration variable.
#' 
#' @param df.samples dataframe with discrete sample results and dates/times
#' @param Conc string column name in df.samples with the concentration results
#' @param sample.time string column name in df.samples with sample dates/times in POSIXct format
#' @param Conc2liters numeric conversion factor that converts the concentrations to a units/liter
#' @param df.Q dataframe with Q and date/time
#' @param Q string name of column in dfQ with Q
#' @param Q.time string name of column in dfQ with date/time in POSIXct format
#' @param Q2liters numeric conversion factor that converts flow to rate per liters
#' @param df.events dataframe with begin and end dates defining the event period
#' @param event.bdate character string with name of variable defining beginning date for events in POSIXct format
#' @param event.edate character string with name of variable defining end date for events in POSIXct format
#' @examples
#' WQdata <- WQdata
#' flowData <- flowData
#' events <- events
#' LoadCompEvent(df.samples=WQdata,Conc="Total_P",sample.time="dateTime",Conc2liters=1,
#' df.Q=flowData,Q="Q",Q.time="pdate",Q2liters=28.3168466,
#' df.events=events,event.bdate="pbdate",event.edate="pedate")
#' @export
#' @return df.load

LoadCompEvent <- function(df.samples,Conc,sample.time,Conc2liters,
                          df.Q,Q,Q.time,Q2liters,
                          df.events,event.bdate,event.edate){
  
  numEvents <- dim(df.events)[1]
  event.loads <- numeric()
  n.samples <- numeric()
  
  for (j in 1:numEvents){
    # j<-1
    event.begin <- df.events[j,event.bdate]
    event.end <- df.events[j,event.edate]
    sub.df.samples <- subset(df.samples,df.samples[,sample.time]>event.begin & 
                               df.samples[,sample.time]<event.end)
    sub.df.samples$event <- j
    n <- dim(sub.df.samples)[1]
    for (i in 1:n){
      
      #Determine begin and end dates and times for individual samples
      #
      if (i == 1){
        
        #begin time for first sample is beginning of event from events file
        bpdate <- event.begin
        epdate <- mean(sub.df.samples[,sample.time][i:(i+1)])
      }else{bpdate <- c(bpdate,mean(sub.df.samples[,sample.time][(i-1):i]))
      }
      if(n>1){
        if(i==n){
          #estimate end time for last sample to be 1/2 of the time between the
          #last sample and the penultimate sample added to the last sample
          epdate <- c(epdate,event.end)
        }else{
          if(i!=1){
            epdate <- c(epdate,mean(sub.df.samples[,sample.time][i:(i+1)]))
          }
        }
      }else{epdate <- event.end}
    }
    
    sub.df.samples <- cbind(sub.df.samples,bpdate)
    sub.df.samples <- cbind(sub.df.samples,epdate)
    sub.df.samples <- Hydrovol(dfQ=df.Q,Q=Q,time=Q.time,
                               df.dates=sub.df.samples,bdate="bpdate",edate="epdate",
                               volume="volume",Qmax="Qmax",duration="duration")
    if(j==1) {df.samples.hydro <- sub.df.samples
    }else df.samples.hydro <- rbind(df.samples.hydro,sub.df.samples)
    n.samples <- c(n.samples,n)
  }
  
  df.samples.hydro$loads <- (df.samples.hydro[,Conc] * Conc2liters) * (df.samples.hydro$volume * Q2liters)
  event.loads <- aggregate(loads~event,data=df.samples.hydro,sum)[2]
  event.volumes <- aggregate(volume~event,data=df.samples.hydro,sum)[2]
  mean.conc <- event.loads/(event.volumes*Q2liters)/Conc2liters
  
  names(event.loads) <- paste("Load.",Conc,sep="")
  names(mean.conc) <- paste("FW.Mean.C.",Conc,sep="")
  
  df.load <- cbind(df.events,data.frame(load=event.loads,concentration=mean.conc,numSamples=n.samples))
  return(df.load)
}


