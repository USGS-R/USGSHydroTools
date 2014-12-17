#' Compute various time-series summary statistics between specified time periods
#' 
#' Compute various stats for time series data over a period of time
#' Can be used for time series data with equally spaced time increments. 
#' File format must include the POSIXct formatted date  and columns of values 
#' with the time series data
#'
#' @param df dataframe with unit values values and date/time in POSIX
#' @param date string name of POSIX date column 
#' @param varname string column with unit values in df
#' @param dates dataframe with sample dates
#' @param starttime string  Column in sample dates data fram with dates in POSIX format 
#' used for extracting summary data from dates dataframe
#' This date serves as the beginning date of the summary period, default is "psdate"
#' @param endtime string Column in sample dates data fram with dates in POSIX format
#' used for extracting summary data from dates dataframe
#' This date serves as the ending date of the summary period, default is "pedate"
#' @param stats.return string vector Options include = c("mean","max","min","median","sum")  
#' specification of stats to apply to the time series data. Current options include mean, max, min, 
#' median, sum, difference, nearest, and nearprev.
#' difference is the latest minus the first value,
#' nearest is the closest value in time to starttime,
#' nearprev is the closest value previous to starttime,
#' nearest and nearprev require a 0 in the times vector.
#' @param subdfvar string subset df data frame by a value in this column
#' @param subdfvalue string value to use in subsetting df 
#' @param subdatesvar string subset dates data frame by a value in this column
#' @param subdatesvalue string value to use in subsetting
#' @param out.varname string variable name for resulting column
#' @examples
#' flowData <- flowData
#' sampleData <- sampleData
#' TSstormstats(df=flowData,date="pdate",varname="Q",
#'       dates=sampleData,starttime="Hbpdate",endtime="Hepdate",
#'       stats.return=c("mean","max","sd"),out.varname="Q")
#' @export
#' @return dates dataframe
TSstormstats <- function(df,                #Unit values file
                         date="pdate",           #Date column in POSIX format in unit values file
                         varname,                #Column name with unit values
                         dates,                  #File with sample dates
                         starttime="Ebpdate",    #Column in sample dates file with start dates in POSIX format
                         endtime="Eepdate",      #Column in sample dates file with end dates in POSIX format
                         stats.return=c("mean"), #Options include "mean","max","min",
                         #"median","sum","sd","maxdiff","difference",
                         #"nearest","nearprev"
                         #maxdiff is the maximum value minus the minimum value for the time period
                         #difference is the latest minus the first value
                         #nearest is the closest value in time
                         #nearprev is the closest value previous to the specified time
                         #nearest and nearprev require a 0 in the times vector
                         subdfvar="",            #variable in UVdf with names of parameters
                         subdfvalue="",          #Optional: value of varname to use in subsetting df 
                         subdatesvar="",         #Optional: subset dates data frame by a value in this column
                         subdatesvalue="",       #Optional: value to use in subsetting
                         out.varname="") {
  
  
  #Initialize Statistical processes 
  stats.names <- c("mean","max","min","median","sum","sd","maxdiff","difference","nearest","nearprev")
  stats.get <- data.frame(row.names=1)
  stats.get[,stats.names[1:length(stats.names)]] <- FALSE
  nstats <- length(stats.return)
  stats.get[,stats.return[1:nstats]] <- TRUE
  stats.return <- names(stats.get[which(stats.get[1,]==TRUE)])
  
  #subset df data frame
  if(subdfvar != "") df <- subset(df,df[,subdfvar]==subdfvalue)
  
  #subset dates data frame
  if(subdatesvar != "") dates <- subset(dates,dates[,subdatesvar]==subdatesvalue)
  
  #initialize varsum vector
  maxrows <- nrow(dates)
  varstats=data.frame(row.names=1:maxrows)
  resultname=vector(mode="character")
  
  varcols <- which(names(df)==varname)
  
  # Compute stats for all identified variables (columns)
  for(k in 1:length(varcols)){
    varname <- names(df)[varcols[k]]
    
    # Compute storm stats for specified periods for each date in the sample dates file
    for (i in 1:maxrows){
      
      subdata <- df[which(df[,date]>= dates[i,starttime]
                          & df[,date] <= dates[i,endtime]),]
      
      if(stats.get[,"mean"]) varstats[i,"mean"] <- mean(subdata[,varname],na.rm=T)
      if(stats.get[,"max"]) varstats[i,"max"] <- max(subdata[,varname],na.rm=T)
      if(stats.get[,"min"]) varstats[i,"min"] <- min(subdata[,varname],na.rm=T)
      if(stats.get[,"median"]) varstats[i,"median"] <- median(subdata[,varname],na.rm=T)
      if(stats.get[,"sum"]) varstats[i,"sum"] <- sum(subdata[,varname],na.rm=T)
      if(stats.get[,"sd"]) varstats[i,"sd"] <- sd(subdata[,varname],na.rm=T)
      if(stats.get[,"maxdiff"]) varstats[i,"maxdiff"] <- max(subdata[,varname],na.rm=T)-min(subdata[,varname],na.rm=T)
      if(stats.get[,"difference"]) varstats[i,"difference"] <- subdata[nrow(subdata),varname] - subdata[1,varname]
      
      statsnames <- stats.return
    }
    rm(resultname)
    resultname <- paste(out.varname,"_",stats.return,sep="")
    names(varstats) <- resultname
    #dates<-"test"
    
    dates <- cbind(dates,varstats)
    varstats <- varstats[,-(1:length(varstats))]
    
  }
  return(dates)
}