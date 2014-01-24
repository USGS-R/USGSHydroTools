#' Mapping data with variable color to define spatial data
#' 
#' Mapping routine that displays spatial data variability by color differences.
#' over layers with political boundaries, hydrologic polygons, and hydrologic lines.
#' 
#' @param df Dataframe with a column that has data to define symbol color,
#' latitude, and longitude of points to be plotted
#' @param colorVar Column name in df to define symbol color
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
#' @param col1 Symbol color for 0-0.25 quantile bin
#' @param col2 Symbol color for 0.25-0.5 quantile bin
#' @param col3 Symbol color for 0.5-0.75 quantile bin
#' @param col4 Symbol color for 0.75-1.0 quantile bin
#' @param xleft Placement of left side of legend box (min latitude)
#' @param xright Placement of right side of legend box (min latitude)
#' @param ybottom  Placement of bottom side of legend box (min longitude)
#' @param ytop  Placement of top side of legend box (min longitude)
#' @param mainTitle Text to be used as the title of the plot
#' @param units 1-4 are for water concentration: mg/L, ug/L, ng/L, pg/L respectively.
#' 5-6 are for sediment concentration: mg/kg, ug/kg, ng/kg, pg/kg respectively.
#' @keywords map spatial color
#' @return NULL
#' @import rgdal
#' @import sp
#' @export
#' @examples
#' lat.dd <- stationINFO$dec.lat.va
#' lon.dd <-  stationINFO$dec.long.va
#' y <- runif(n=length(lat.dd),min=0,max=50)
#' df <- data.frame(y=y, lat.dd=lat.dd,lon.dd=lon.dd)
#' colorVar <- "y"
#' latVar <- "lat.dd"
#' lonVar <- "lon.dd"
#' politicalBounds <- shape_poliboundsClip
#' hydroPolygons <- subShape_hydropolyClip
#' hydroLines <- shape_hydrolineClip
#' xmin <- -96.5
#' xmax <- -72
#' ymin <- 40.5
#' ymax <- 49.5
#' xleft <- -95
#' ybottom <- 40.7
#' xright <- -90.8
#' ytop <- 43.5
#' col1 <- "tan"
#' col2 <- "orange3"
#' col3 <- "orangered1"
#' col4 <- "orangered4"
#' mainTitle <- "OC Pesticides"
#' MapColor(df,colorVar,latVar,lonVar,
#'          politicalBounds,hydroPolygons,hydroLines,
#'          xmin,xmax,ymin,ymax,xleft=xleft,xright=xright,ytop=ytop,ybottom=ybottom,mainTitle=mainTitle)
MapColor <- function(df,colorVar,latVar,lonVar,
                     politicalBounds,hydroPolygons,hydroLines,
                     xmin,xmax,ymin,ymax,
                     col1="tan",col2="orange3",col3="orangered1",col4="orangered4",
                     xleft,xright,ytop,ybottom,
                     mainTitle="",units=2){
  
  #set plot parameters
  par( mar=c(0,0,1,0), new = FALSE,xpd=NA)#,mgp=c(3,0.1,0))
  
  #Choose plot color bins: 
  #Use 0.25, 0.5, and 0.75 quantiles of non-zero values to define bins
  which(df[,colorVar] != 0)
  binThresh <- quantile(df[which(df[,colorVar] != 0),colorVar],c(0.25,0.5,0.75))
  binCol <- c(col2,col3,col4)
  plotSymbol <- 21 
  
  fillCol <- rep(col1,dim(df)[1])
  for (i in 1:length(binThresh)) fillCol <- ifelse(df[,colorVar] > binThresh[i],binCol[i],fillCol)
  plot(politicalBounds,col="gray90",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
  plot(hydroPolygons,col="lightskyblue2",xlim=c(xmin,xmax),ylim=c(ymin,ymax),add=TRUE)#
  lines(hydroLines,col="lightskyblue2",xlim=c(xmin,xmax),ylim=c(ymin,ymax))#
  plot(politicalBounds,add=TRUE)
  points(df[,lonVar], df[,latVar],pch=plotSymbol, col="black",bg=fillCol,cex=1.2)
  mtext(mainTitle,side=3,line=-4,outer=TRUE,font=2,cex=1.3)
  
  
  legendTextCex <- 0.9
  rect(xleft=xleft,ybottom=ybottom,xright=xright,ytop=ytop,col="white")
  binThresh <- round(binThresh,3)
  legendText= c(paste("<",binThresh[1]),
                paste(binThresh[1],"-",binThresh[2]),
                paste(binThresh[2],"-",binThresh[3]),
                paste(binThresh[3],"-",round(max(df[,colorVar]),3)))
  legendCol = binCol
  
  startText <- c(xleft+0.2,ytop-0.2)
  startText <- c((xleft+xright)/2,ytop-0.2)
  text("Color of symbol",x=startText[1],y=startText[2],font=2,cex=legendTextCex)
  text("indicates",x=startText[1],y=startText[2]-0.3,font=2,cex=legendTextCex)
  
  if(units==1) concText <- expression(bold(paste("concentration (","mg/L)",sep="")))
  if(units==2) concText <- expression(bold(paste("concentration (",mu,"g/L)",sep="")))
  if(units==3) concText <- expression(bold(paste("concentration (","ng/L)",sep="")))
  if(units==4) concText <- expression(bold(paste("concentration (","pg/L)",sep="")))
  if(units==5) concText <- expression(bold(paste("concentration (","mg/kg)",sep="")))
  if(units==6) concText <- expression(bold(paste("concentration (",mu,"g/kg)",sep="")))
  if(units==7) concText <- expression(bold(paste("concentration (","ng/kg)",sep="")))
  if(units==8) concText <- expression(bold(paste("concentration (","pg/kg)",sep=""))) 
  
  
  text(concText,x=startText[1],y=startText[2]-0.6,font=2,cex=legendTextCex)
#   text(expression(bold(paste("concentration (",mu,"g/L)",sep=""))),x=startText[1],y=startText[2]-0.6,
#        font=2,cex=legendTextCex)
  
  legend(x=xleft+0.2,y=ytop-1.,legendText,pt.bg=c("tan",binCol),pch=plotSymbol,bg="white",pt.cex=1.5,bty="n")
}

