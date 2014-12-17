#' Mapping data with variable color to define spatial data
#' 
#' Mapping routine that displays spatial data variability by color differences.
#' over layers with political boundaries, hydrologic polygons, and hydrologic lines.
#' 
#' @param df Dataframe with 3 required columns to define symbol color,
#' latitude, and longitude of points to be plotted, and optionally 5 addtional columns: label names, 
#' offset for latitude and longitude for labels, and offset of latitude and longitude for the ending
#' @param colorVar Column name in df to define symbol color
#' @param latVar Column name in df to define latitude
#' @param lonVar Column name in df to define longitude
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
#' @param LegCex size of the text and symbols in the legend as numeric. Assigns the
#' pt.cex and cex arguments in legend() and text().
#' @param DL numeric vector of detection limits
#' @param titlePos position of title as numeric. Assigns the line() argument in mtext(). 
#' @keywords map spatial color
#' @return NULL
#' @import rgdal
#' @import sp
#' @export
#' @examples
#' lat <- SI$lat
#' lon <-  SI$lon
#' y <- runif(n=length(lat),min=0,max=50)
#' df <- data.frame(y=y, lat=lat,lon=lon)
#' colorVar <- "y"
#' latVar <- "lat"
#' lonVar <- "lon"
#' DL <- c(rep(0.05,times=19),rep(0.04,times=10))
#' LegCex <- 0.7
#' titlePos <- -4
#' 
#' xmin <- -96.5
#' xmax <- -72
#' ymin <- 40.5
#' ymax <- 49.5
#' xleft <- -95
#' ybottom <- 40.7
#' xright <- -90.8
#' ytop <- 43.5
#' mainTitle <- "OC Pesticides"
#' 
#' #Without labels
#' 
#' #Example works best in a landscape view:
#' pdf("GreatLakesExamplePlotNoLabels.pdf",width=11,height=8)
#' MapColor(df,colorVar, latVar, lonVar,
#'          xmin,xmax,ymin,ymax,
#'          xleft=xleft, xright=xright, ytop=ytop, ybottom=ybottom,
#'          mainTitle=mainTitle,
#'          includeLabels=FALSE, DL=DL, LegCex=LegCex, titlePos=titlePos)
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
#' MapColor(df,colorVar,latVar,lonVar,
#'          xmin,xmax,ymin,ymax,
#'          xleft=xleft,xright=xright,ytop=ytop,ybottom=ybottom,
#'          mainTitle=mainTitle, includeLabels=TRUE,
#'          labels=labelVar, offsetLat=offsetLatVar, 
#'          offsetLon=offsetLonVar, offsetLineLat=offsetLineLatVar,
#'          offsetLineLon=offsetLineLonVar, DL=DL, LegCex=LegCex, titlePos=titlePos)
#'dev.off()
#'#To view the produced plot, us the following command:
#'\dontrun{shell.exec("GreatLakesExamplePlot.pdf")}
MapColor <- function(df,colorVar,latVar,lonVar,
                     xmin,xmax,ymin,ymax,
                     col1="tan",col2="orange3",col3="orangered1",col4="orangered4",
                     xleft,xright,ytop,ybottom,mainTitle="",units=6,includeLabels=FALSE,
                     labels="",offsetLat="",offsetLon="",offsetLineLat="",offsetLineLon="",DL=0,LegCex=0.9,titlePos=-4){
  
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
  fillCol[which(is.na(fillCol)==TRUE)] <- "white"
  
  retList <- clipShape(xmin,xmax,ymin,ymax)
  plotBackgroundMap(retList)
  
  if(includeLabels){
    MapLabels(df=df,labels=labels,dataLat=latVar,dataLon=lonVar,
              offsetLat=offsetLat,offsetLon=offsetLon,
              offsetLineLat=offsetLineLat,
              offsetLineLon=offsetLineLon,
              cex=0.75)
  }
  
  points(df[,lonVar], df[,latVar],pch=plotSymbol, col="black",bg=fillCol,cex=1.2)
  mtext(mainTitle,side=3,line=titlePos,outer=TRUE,font=2,cex=1.3)
  rect(xleft=xleft,ybottom=ybottom,xright=xright,ytop=ytop,col="white")
  binThresh <- round(binThresh,3)
  
  if(all(fillCol=="white")){
    legendText <- paste("ND","(",min(DL),"-",max(DL),")")
    pt.bgCol <-c("white")
  } else {  
    legendText <- c(paste("ND","(",min(DL),"-",max(DL),")"),
                    paste(max(DL),"-",binThresh[1]),
                    paste(binThresh[1],"-",binThresh[2]),
                    paste(binThresh[2],"-",binThresh[3]),
                    paste(binThresh[3],"-",round(max(df[,colorVar],na.rm = TRUE),3)))
    pt.bgCol <- c("white","tan",binCol)
  }
  
  legendCol = binCol
  startText <- c(xleft+0.2,ytop-0.2)
  startText <- c((xleft+xright)/2,ytop-0.2)
  
  if(units==1) concText <- expression(bold(paste("concentration (","mg/L)",sep="")))
  if(units==2) concText <- expression(bold(paste("concentration (",mu,"g/L)",sep="")))
  if(units==3) concText <- expression(bold(paste("concentration (","ng/L)",sep="")))
  if(units==4) concText <- expression(bold(paste("concentration (","pg/L)",sep="")))
  if(units==5) concText <- expression(bold(paste("concentration (","mg/kg)",sep="")))
  if(units==6) concText <- expression(bold(paste("concentration (",mu,"g/kg)",sep="")))
  if(units==7) concText <- expression(bold(paste("concentration (","ng/kg)",sep="")))
  if(units==8) concText <- expression(bold(paste("concentration (","pg/kg)",sep=""))) 
  
  text(concText,x=startText[1],y=startText[2]-0.6,font=2,cex=LegCex)
  #   text(expression(bold(paste("concentration (",mu,"g/L)",sep=""))),x=startText[1],y=startText[2]-0.6,
  #        font=2,cex=legendTextCex)
  
  legend(x=xleft+0.2,y=ytop-1.,legendText,pt.bg=pt.bgCol,pch=plotSymbol,bg="white",cex=LegCex,pt.cex=LegCex,bty="n")
}