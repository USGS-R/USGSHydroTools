#' Compute various antecedent summary statistics from time series data for specified windows of time
#' 
#' @description
#' Compute various stats for time series data over a period of time
#' Originally scripted for NOAA Great Lakes model from GDP for given set of dates and 
#' time periods, but could be used for any time series. File format must include the 
#' POSIX formatted date (yyyy-mm-ddThh:mm:ssZ), and then columns of values with the 
#' time series data
#'
#' read date with format mm/dd/yy hh:mm (use %Y if the format includes YYYY instead of yy)
#' koepkeSM$date <- as.POSIXct(koepkeSM$Date,"%m/%d/%y %H:%M")
#'
#' read date with format mm/dd/yyyy hh:mm
#' cedardates$psdate <- as.POSIXct(cedardates$Startdate,"%m/%d/%Y %H:%M")
#' cedardates$parfdate <- as.POSIXct(cedardates$Enddate,"%m/%d/%Y %H:%M")
#'
#' Subset the data by begin and end date (can also assign to a df if you like)
#' then define min mean median and max for the subset. Do this for all date periods
#' in the file.
#'
#' @param df dataframe Unit values file
#' @param date string Date column in POSIX format in unit values file
#' @param varnames string Column name with unit values
#' @param dates dataframe File with sample dates
#' @param starttime string Column in sample dates file with dates in POSIX format, defaults to "psdate"
#' @param times vector to define desired processing times. Zero indicates then nearest or nearest previous value.
#' Default is hours, but can be specified using "units" variable
#' @param units string Units of times vector. Can be any of the following: "minutes","min","mins","hours","hr","hrs","day","days","week","weeks"
#' @param stats.return string Options include "mean","max","min","median","sum","sd","maxdiff","difference",nearest","nearprev"
#' maxdiff is the maximum value minus the minimum value for the time period,
#' difference is the latest minus the first value,
#' nearest is the closest value in time,
#' nearprev is the closest value previous to the specified time,
#' nearest and nearprev require a 0 in the times vector,
#' @param subdfvar string column name in UVdf with names of parameters, default is ""
#' @param subdfvalue string Optional: value of varname to use in subsetting df, default is ""
#' @param subdatesvar string Optional: subset dates data frame by a value in this column, default is ""
#' @param subdatesvalue string Optional: value to use in subsetting
#' @param out.varname string
#' @return dates dataframe
#' @examples
#' flowData <- flowData
#' sampleData <- sampleData
#' TSstats(df=flowData,date="pdate",varnames="Q",
#'         dates=sampleData,starttime="Hbpdate",times=c(1,3,6,12,24),
#'         units="hrs",stats.return=c("mean","max","sd"),out.varname="Q")
#' @export
TSstats <- function(df,                     #Unit values file
                    date="date",            #Date column in POSIX format in unit values file
                    varnames,               #Column name with unit values
                    dates,                  #File with sample dates
                    starttime="psdate",     #Column in sample dates file with dates in POSIX format
                    times=c(1,2),           #Vector to define desired processing times
                    #Zero indicates then nearest or nearest previous value
                    #Default is hours, but can be specified
                    #using "units" variable
                    units="hours",          #Units of times vector. Can be any of the following:
                    #"minutes","min","mins","hours","hr","hrs","day","days","week","weeks"
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
                    out.varname=""){
  
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
  
  #Convert times to hours
  unit.options <- c("minutes","min","mins","hours","hr","hrs","day","days","week","weeks")
  hour.conversions <- c(rep(1/60,3),rep(1,3),24,24,rep(24*7,2))
  timeHrs <- times*hour.conversions[unit.options==units]
  
  #initialize varsum vector
  maxrows <- nrow(dates)
  varstats=data.frame(row.names=1:maxrows)
  resultname=vector(mode="character")
  
  varcols <- which(names(df)==varnames)
  
  # Compute stats for all identified variables (columns)
  for(k in 1:length(varcols)){
    varname <- names(df)[varcols[k]]
    
    # compute the  stats for all identified durations
    for(j in 1:length(timeHrs)) {      
      dates$parfdate <- dates[,starttime] - timeHrs[j]*60*60
      
      # Compute antecedent stats for specified periods for each date in the sample dates file
      for (i in 1:maxrows){
        
        if(timeHrs[j]>0){
          subdata <- df[which(df[,date]>= dates[i,"parfdate"]
                              & df[,date] < dates[i,starttime]),]
          
          if(stats.get[,"mean"]) varstats[i,"mean"] <- mean(subdata[,varname],na.rm=T)
          if(stats.get[,"max"]) varstats[i,"max"] <- max(subdata[,varname],na.rm=T)
          if(stats.get[,"min"]) varstats[i,"min"] <- min(subdata[,varname],na.rm=T)
          if(stats.get[,"median"]) varstats[i,"median"] <- median(subdata[,varname],na.rm=T)
          if(stats.get[,"sum"]) varstats[i,"sum"] <- sum(subdata[,varname],na.rm=T)
          if(stats.get[,"sd"]) varstats[i,"sd"] <- sd(subdata[,varname],na.rm=T)
          if(stats.get[,"maxdiff"]) varstats[i,"maxdiff"] <- max(subdata[,varname],na.rm=T)-min(subdata[,varname],na.rm=T)
          if(stats.get[,"difference"]) varstats[i,"difference"] <- subdata[nrow(subdata),varname] - subdata[1,varname]
        } else {
          if(stats.get[,"nearprev"]) {
            subdata <- df[which(df[,date] < dates[i,starttime]),]
            varstats[i,"nearprev"] <- (subdata[nrow(subdata),varname])
          }
          
          if(stats.get[,"nearest"]){
            time.diffs <- as.numeric(difftime(dates[i,starttime],df[,date]))
            nearest.data <- which(abs(time.diffs)==min(abs(time.diffs)))[1]
            varstats[i,"nearest"] <- df[nearest.data,varname]
          }
        }
        
      }
      
      if(timeHrs[j]==0){
        statsnames <- which((stats.return[1:nstats]== "nearprev") 
                            | (stats.return[1:nstats] == "nearest"))
        stats.return[statsnames]
      } else {
        statsnames <- which((stats.return[1:nstats]!= "nearprev") 
                            & (stats.return[1:nstats] != "nearest"))
      }
      rm(resultname)
      
      if(length(statsnames)>0) {
        if(times[j]>0){
          resultname <- paste(out.varname[k],"_",stats.return[statsnames],times[j],sep="")
        }else resultname <- paste(out.varname[k],"_",stats.return[statsnames],sep="")
        
        names(varstats) <- resultname
        dates <- cbind(dates,varstats)
        varstats <- varstats[,-(1:length(varstats))]
      }
    }
  }
  return(dates)
}