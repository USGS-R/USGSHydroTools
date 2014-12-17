#' Mapping data with variable color to define spatial data
#' 
#' Mapping routine that displays spatial data variability by color differences.
#' over layers with political boundaries, hydrologic polygons, and hydrologic lines.
#' 
#' @param df Dataframe with a column that has data to define
#' latitude and longitude of points to be plotted, offset for latitude and 
#' longitude for labels, and offset of latitude and longitude for the ending
#' point of lines pointing to the labels if needed
#' @param labels String variable in dataframe df with label names
#' @param dataLat Latitude of points that were plotted on the map
#' @param dataLon Longitude of points that were plotted on the map
#' @param offsetLat Variable in dataframe df for the offset from dataLat used 
#' for label positioning
#' @param offsetLon Variable in dataframe df for the offset from dataLon used 
#' for label positioning
#' @param offsetLineLat Variable in dataframe df for the offset from dataLat used to
#' position the end of the line drawn to the label. Lines are optional.
#' @param offsetLineLon Variable in dataframe df for the offset from dataLon used to
#' position the end of the line drawn to the label. Lines are optional.
#' @param \dots Optional graphical parameters.
#' @export
#' @examples
#' lat.dd <- SI$lat
#' lon.dd <-  SI$lon
#' y <- runif(n=length(lat.dd),min=0,max=50)
#' df <- data.frame(y=y, lat.dd=lat.dd,lon.dd=lon.dd)
#' colorVar <- "y"
#' latVar <- "lat.dd"
#' lonVar <- "lon.dd"
#' xmin <- -96.5
#' xmax <- -72
#' ymin <- 40.5
#' ymax <- 49.5
#' xleft <- -95
#' ybottom <- 40.7
#' xright <- -90.8
#' ytop <- 43.5
#' DL <- c(rep(0.05,times=19),rep(0.04,times=10))
#' LegCex <- 0.7
#' titlePos <- -4
#' mainTitle <- "OC Pesticides"
#' pdf("GreatLakesPlot.pdf", width=11, height=8)
#' MapColor(df,colorVar,latVar,lonVar,
#'           xmin,xmax,ymin,ymax,
#'           xleft=xleft,xright=xright,ytop=ytop,ybottom=ybottom,
#'           mainTitle=mainTitle,includeLabels=FALSE,
#'           DL=DL,LegCex=LegCex,titlePos=titlePos)
#' MapLabels(df=SI,labels="Site",dataLat="lat",dataLon="lon",
#'   offsetLat="offsetLat",offsetLon="offsetLon",
#'   offsetLineLat="offsetLineLat",
#'   offsetLineLon="offsetLineLon",
#'   cex=0.75)
#'dev.off()
#'# To see the created pdf, use the following commend:
#'\dontrun{shell.exec("GreatLakesPlot.pdf")}
MapLabels <- function(df,labels,dataLat,dataLon,offsetLat,offsetLon,offsetLineLat,offsetLineLon,...){#labelcex=0.75){
  
  labelLat <- df[,dataLat]+df[,offsetLat]
  labelLon <- df[,dataLon]+df[,offsetLon]
  df[,labels] <- as.character(df[,labels])
  labelPos <- 1
  
  text(x=labelLon,y=labelLat,labels=df[,labels],...)
    
  ifelse(is.na(df[,offsetLineLat]),0,df[,offsetLineLat])
  ifelse(is.na(df[,offsetLineLon]),0,df[,offsetLineLon])
  
  noLines <- which(abs(df[,offsetLineLat])+abs(df[,offsetLineLon])==0)
  df[noLines,offsetLineLat] <- NA
  df[noLines,offsetLineLon] <- NA
  
  lineLat <- df[,dataLat]+df[,offsetLineLat]
  lineLon <- df[,dataLon]+df[,offsetLineLon]
  
  
  segments(x0=df[,dataLon],y0=df[,dataLat],x1=lineLon,y1=lineLat)
  
}
