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
#' @param politicalBounds Shapefile of class "SpatialPolygonsDataFrame" for 
#' defining political boundaries
#' @param hydroPolygons Shapefile of class "SpatialPolygonsDataFrame" for 
#' defining hydrologic polygons (lakes)
#' @param hydroLines shapefile of class "SpatialLinesDataFrame" for 
#' defining hydrologic lines (rivers/streams)
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
#' politicalBounds <- shape_poliboundsClip
#' hydroPolygons <- subShape_hydropolyClip
#' hydroLines <- shape_hydrolineClip
#' xmin <- -96.5
#' xmax <- -72
#' ymin <- 40.5
#' ymax <- 49.5
#' mainTitle <- "Site Locations"
#' 
#' #Without labels
#' 
#' #Example works best in a landscape view:
#' pdf("GreatLakesExamplePlotNoLabels.pdf",width=11,height=8)
#' MapLocations(df,latVar,lonVar,
#'              politicalBounds,hydroPolygons,hydroLines,
#'              xmin,xmax,ymin,ymax,mainTitle=mainTitle,
#'              includeLabels=FALSE)
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
#'              politicalBounds,hydroPolygons,hydroLines,
#'              xmin,xmax,ymin,ymax,mainTitle=mainTitle,includeLabels=TRUE,
#'              labels=labelVar, offsetLat=offsetLatVar, offsetLon=offsetLonVar,offsetLineLat=offsetLineLatVar,
#'              offsetLineLon=offsetLineLonVar)
#'dev.off()
#'#To view the produced plot, us the following command:
#'\dontrun{shell.exec("GreatLakesExamplePlot.pdf")}
MapLocations <- function(df,latVar,lonVar,
                         politicalBounds,hydroPolygons,hydroLines,
                         xmin,xmax,ymin,ymax,
                         col1="tan",
                         mainTitle="",includeLabels,
                         labels="",offsetLat="",offsetLon="",offsetLineLat="",offsetLineLon=""){
  
  #set plot parameters
  par( mar=c(0,0,1,0), new = FALSE,xpd=NA)#,mgp=c(3,0.1,0))
  
  plotSymbol <- 21 
  
  fillCol <- rep(col1,dim(df)[1])
  plot(politicalBounds,col="gray90",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
  plot(hydroPolygons,col="lightskyblue2",xlim=c(xmin,xmax),ylim=c(ymin,ymax),add=TRUE)#
  lines(hydroLines,col="lightskyblue2",xlim=c(xmin,xmax),ylim=c(ymin,ymax))#
  plot(politicalBounds,add=TRUE)
  
  if(includeLabels){
    MapLabels(df=df,labels=labels,dataLat=latVar,dataLon=lonVar,
              offsetLat=offsetLat,offsetLon=offsetLon,
              offsetLineLat=offsetLineLat,
              offsetLineLon=offsetLineLon,
              cex=0.75)
  }
  
  points(df[,lonVar], df[,latVar],pch=plotSymbol, col="black",bg=col1,cex=1.2)
  mtext(mainTitle,side=3,line=-4,outer=TRUE,font=2,cex=1.3)
  
  
}

