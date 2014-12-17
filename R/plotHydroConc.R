#' plotHydroConc
#'
#' Function to generate a two panel graph with hydrograph(s) in the 
#' top panel and concentration or other water quality parameter (e.g. flux) 
#' in the lower panel with corresponding date axes for the top and bottom panels
#' 
#'@param Q dataframe list with flow variables and POSIXct date variables. The 
#'list contains one file per Q record (usually one dataframe per site).
#'@param QVars vector of strings that signify the column names that represent 
#'the flow variables in the dataframes defined in Q
#'@param QDateVars vector of strings that signify the column names that represent 
#'the POSIXct date variables in the dataframes defined in Q
#'@param smooth Boolean vector to trigger lowess smooth in graphing rather than
#'direct flow graphing (useful for sites impacted by seiche)
#'@param sites Sites for Q data. This will be used in the legend of the Q panel
#'@param Conc dataframe with variables to be plotted in the second panel
#'@param CVars vector of strings that represent the variables in Conc to include
#'in the graphs in the second panel
#'@param CDateVar column name in Conc for sample dates and times in POSIXct
#'#'@param CVarsDisplay variable names from CVars to be displayed on the legend
#'@param dates dataframe with beginning and ending sample dates and times 
#'and beginning and ending hydrograph dates and times
#'@param sampDates vector of length two that contains strings representing
#'the column names for beginning and ending dates and times for samples
#'@param eventDates vector of length two that contains strings representing
#'the column names for beginning and ending dates and times for the sampling 
#'event. This is the hydrograph segment that the samples are intended to represent
#'(often a runoff hydrograph or a baseflow period)
#'@param Qcols vector of colors for ploting hydrographs from the Q dataframe
#'@param Ccols vector of colors for plotting from the Conc dataframe
#'@param leftBuffer time in days to plot before the beginning of the event period
#'@param rightBuffer time in days to plot after the end of the event period
#'@param concLines Boolean variable to signify whether a line should be drawn 
#'between consecutive data points from the Conc dataframe in the second graph 
#'panel
#'@param Qylab y-axis label for the first graph panel
#'@param Cylab y-axis label for the second graph panel
#'@param title1 Line 1 of plot title
#'@param title2 Line 2 of plot title
#'@export
#'@examples
#'#Add example
#'
plotHydroConc <- function(Q,QVars,QDateVars,smooth,sites,
                          Conc,CVars,CDateVar,CVarsDisplay,dates,
                          sampDates,eventDates,Qcols,Ccols,
                          leftBuffer,rightBuffer,concLines=TRUE,
                          Qylab="Discharge (cfs)",Cylab="Concentration",
                          title1="Flow",title2="and concentration"){
  
  # Define plot layout: panel 1 for Q and panel 2 for FIB
  mylayout <- matrix(c(1,
                       1,
                       1,
                       2,
                       2),5,1,byrow=TRUE)
  layout(mylayout)
  
  for (i in 1:nrow(dates)) {
    
    ####  Plot Q for all sites on first Graph  ####
    
    xlim=c(dates[i,eventDates[1]] - leftBuffer*24*3600,dates[i,eventDates[2]] + rightBuffer*24*3600)
    
    #Set Margins for first plot
    par(mar= c(0, 4, 4, 2) + 0.1)
    
    
    #Set up first panel for plotting Q
    q <- Q[[1]]
    ylim <- c(0,0)
    for (j in length(Q):1) {
      q <- Q[[j]]      
      subQ <- q[which(q[,QDateVars[j]] >= xlim[1] & q[,QDateVars[j]] <= xlim[2]),]
      if(smooth[j]) {
        span <- 125/length(subQ[,QVars[j]])
        subQ[,QVars[j]] <- lowess(subQ[,QDateVars[j]],subQ[,QVars[j]],f=span)$y
      }
      ylim <- range(c(ylim,range(subQ[,QVars[j]],na.rm=TRUE)),na.rm=TRUE)
    }
    
    plot(subQ[,QDateVars[1]],subQ[,QVars[1]],
         xaxt="n",
         ylab=Qylab, 
         xlab="",
         ylim=ylim,
         xlim=xlim,
         type="n",
         col=Qcols[1],
         main = "")
    r <- as.POSIXct(round(range(subQ$pdate,na.rm=TRUE), "hours"))
    r <- as.POSIXct(trunc(range(subQ$pdate,na.rm=TRUE), "days"))
    r[2] <- r[2]+24*3600
    rhour <- seq(r[1], r[2], by=24*3600/4)
    r1hour <- seq(r[1], r[2], by=24*3600/24)
    rday <- seq(r[1], r[2], by="days")
    axis.POSIXct(1,subQ$pdate,at=rhour,format=" ",tcl=0.2)
    axis.POSIXct(1,subQ$pdate,at=r1hour,format=" ",tcl=0.1)
    axis.POSIXct(1,subQ$pdate,at=rday,format=" ",tcl=0.5)
    abline(v=c(dates[i,eventDates[1]],dates[i,eventDates[2]]),lty=4,col="orange",lwd=1.5)
    abline(h=0,lty=2)
    
    #Add in lines for hydrographs
    for(k in 1:length(Q)){
      q <- Q[[k]]
      subQ <- q[which(q[,QDateVars[k]] >= xlim[1] & q[,QDateVars[k]] <= xlim[2]),]
      if(smooth[j]) {
        span <- 125/length(subQ[,QVars[j]])
        subQ[,QVars[j]] <- lowess(subQ[,QDateVars[j]],subQ[,QVars[j]],f=span)$y
      }
      lines(subQ[,QDateVars[k]],subQ[,QVars[k]],col=Qcols[k], lwd=1.5)
    }
    # Add Legend
    legend("topleft",
           legend=sites, 
           col=Qcols,
           text.col=Qcols,
           lty=1)
    mtext(side=3,line=2.5,title1)
    mtext(side=3,line=1.0,title2 )
    
    #Set up second panel for plotting concentrations
    
    #Set Margins for second plot
    par(mar= c(5, 4, 0, 2) + 0.1)
    
    subC <- Conc[which(Conc[,CDateVar] >= xlim[1] & Conc[,CDateVar] <= xlim[2]),]
    ylim <- range(subC[,CVars],na.rm=TRUE)
    
    plot(subC[,CDateVar],subC[,CVars[1]],
         xaxt="n",
         ylab=Cylab, 
         xlab="",
         ylim=ylim,
         xlim=xlim,
         type="n",
         col=Ccols[1],
         main = "")
    axis.POSIXct(1,subC$pdate,at=rhour,format=" ",tcl=0.2)
    axis.POSIXct(1,subC$pdate,at=r1hour,format=" ",tcl=0.1)
    axis.POSIXct(1,subC$pdate,at=rday,format=" ",tcl=0.5)
    axis.POSIXct(1,subQ$pdate,format = "%m/%d/%Y")
    
    
    #Add in points for hydrographs
    for(k in 1:length(CVars)){
      points(subC[,CDateVar],subC[,CVars[k]],col=Ccols[k],pch=20)
      if(concLines)lines(subC[,CDateVar],subC[,CVars[k]],col=Ccols[k],lwd=1.5)
    }
    # Add Legend
    legend("topleft",
           legend=CVarsDisplay, 
           col=Ccols,
           text.col=Ccols,
           lty=1)
  }
}
  
  