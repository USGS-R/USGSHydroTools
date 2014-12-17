#' Mapping locations of interst
#' 
#' Mapping routine that displays locations of sites over layers with 
#' political boundaries, hydrologic polygons, and hydrologic lines.
#' 
#' @param df Dataframe with 3 required columns to define symbol color,
#' latitude, and longitude of points to be plotted, and optionally 5 addtional columns: label names, 
#' offset for latitude and longitude for labels, and offset of latitude and longitude for the ending
#' @param latVar Column name in df to define latitude
#' @param lonVar Column name in df to define longitude
#' @param xmin Left longitudinal boundary for plotting
#' @param xmax Right longitudinal boundary for plotting
#' @param ymin Bottom latitudinal boundary for plotting
#' @param ymax Top latitudinal boundary for plotting
#' @param col1 Symbol color for site location
#' @param mainTitle Text to be used as the title of the plot
#' @param includeLabels logical, if TRUE labels will be included on plot.
#' @param labels String variable in dataframe df with label names
#' @param offsetLat Variable in dataframe df for the offset from dataLat used 
#' for label positioning
#' @param offsetLon Variable in dataframe df for the offset from dataLon used 
#' for label positioning
#' @param offsetLineLat Variable in dataframe df for the offset from dataLat used to
#' position the end of the line drawn to the label. Lines are optional.
#' @param offsetLineLon Variable in dataframe df for the offset from dataLon used to
#' position the end of the line drawn to the label. Lines are optional.
#' @param titlePos position of title as numeric. Assigns the line() argument in mtext(). Default is -4.
#' @keywords map spatial color
#' @return NULL
#' @import rgdal
#' @import sp
#' @export
#' @examples
#' lat <- SI$lat
#' lon <-  SI$lon
#' df <- data.frame(lat=lat,lon=lon)
#' latVar <- "lat"
#' lonVar <- "lon"
#'
#' xmin <- -96.5
#' xmax <- -72
#' ymin <- 40.5
#' ymax <- 49.5
#' mainTitle <- "Site Locations"
#' titlePos <- -4
#' 
#' #Without labels
#' 
#' #Example works best in a landscape view:
#' pdf("GreatLakesExamplePlotNoLabels.pdf",width=11,height=8)
#' MapLocations(df,latVar,lonVar,
#'              xmin,xmax,ymin,ymax,mainTitle=mainTitle,
#'              includeLabels=FALSE,titlePos=titlePos)
#'dev.off()
#'#To view the produced plot, us the following command:
#'\dontrun{shell.exec("GreatLakesExamplePlotNoLabels.pdf")}
#'
#'# With labels:
#' df <- merge(df, SI, by=c("lat","lon"))
#' labelVar <- "Site" 
#' offsetLatVar <- "offsetLat"
#' offsetLonVar <- "offsetLon"
#' offsetLineLatVar <- "offsetLineLat"
#' offsetLineLonVar <- "offsetLineLon"

#' #Example works best in a landscape view:
#' pdf("GreatLakesExamplePlot.pdf",width=11,height=8)
#' MapLocations(df,latVar,lonVar,
#'              xmin,xmax,ymin,ymax,mainTitle=mainTitle,includeLabels=TRUE,
#'              labels=labelVar, offsetLat=offsetLatVar, 
#'              offsetLon=offsetLonVar,offsetLineLat=offsetLineLatVar,
#'              offsetLineLon=offsetLineLonVar,titlePos=titlePos)
#'dev.off()
#'#To view the produced plot, us the following command:
#'\dontrun{shell.exec("GreatLakesExamplePlot.pdf")}
MapLocations <- function(df,latVar,lonVar,
                         xmin,xmax,ymin,ymax,
                         col1="tan",
                         mainTitle="",includeLabels,
                         labels="",offsetLat="",offsetLon="",offsetLineLat="",offsetLineLon="",titlePos=-4){
  
  #set plot parameters
  par( mar=c(0,0,1,0), new = FALSE,xpd=NA)#,mgp=c(3,0.1,0))
  
  plotSymbol <- 21 
  
  fillCol <- rep(col1,dim(df)[1])
  
  retList <- clipShape(xmin,xmax,ymin,ymax)
  plotBackgroundMap(retList)
  
  if(includeLabels){
    MapLabels(df=df,labels=labels,dataLat=latVar,dataLon=lonVar,
              offsetLat=offsetLat,offsetLon=offsetLon,
              offsetLineLat=offsetLineLat,
              offsetLineLon=offsetLineLon,
              cex=0.75)
  }
  
  points(df[,lonVar], df[,latVar],pch=plotSymbol, col="black",bg=col1,cex=1.2)
  mtext(mainTitle,side=3,line=titlePos,outer=TRUE,font=2,cex=1.3)
  
  
}

