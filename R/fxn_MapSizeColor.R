#' Mapping data with variable size and color to define spatial data
#' 
#' Mapping routine that displays spatial data variability by size and color differences.
#' over layers with political boundaries, hydrologic polygons, and hydrologic lines.
#' 
#' @param df Dataframe with 4 required columns that have data to define symbol size, color,
#' latitude, and longitude of points to be plotted, and optionally 5 addtional columns: label names, 
#' offset for latitude and longitude for labels, and offset of latitude and longitude for the ending
#' point of lines pointing to the labels if needed
#' @param colorVar Column name in df to define symbol color
#' @param sizeVar Column name in df to define symbol size
#' @param latVar Column name in df to define latitude
#' @param lonVar Column name in df to define longitude
#' @param sizeThresh1 Low threshold value of sizeVar for defining bins
#' @param sizeThresh2 High  threshold value of sizeVar for defining bins
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
#' pt.cex and cex arguments in legend() and text(). Does not change the size of the
#' symbols representing number of samples per site. Default is 0.9/
#' @param titlePos position of title as numeric. Assigns the line() argument in mtext(). Default is -4.
#' @keywords map spatial size color
#' @return NULL
#' @import rgdal
#' @import sp
#' @export
#' @examples
#' lat <- SI$lat
#' lon <-  SI$lon
#' y <- runif(n=length(lat),min=0,max=50)
#' count <- round(runif(n=length(lat),min=0,max=30),0)
#' df <- data.frame(y=y, lat=lat,lon=lon,count=count)
#'
#' colorVar <- "y"
#' sizeVar <- "count"
#' latVar <- "lat"
#' lonVar <- "lon"
#' 
#' xmin <- -96.5
#' xmax <- -72
#' ymin <- 40.5
#' ymax <- 49.5
#' xleft <- -95
#' ybottom <- 40.4
#' xright <- -90.8
#' ytop <- 45.3
#' sizeThresh1 <- 2
#' sizeThresh2 <- 14
#' LegCex <- 0.9
#' mainTitle <- "Colors vary by concentration"
#' titlePos <- -2
#' 
#' 
#'# Without labels:
#' 
#' #Example works best in a landscape view:
#' pdf("GreatLakesExamplePlotNoLabels.pdf",width=11,height=8)
#' MapSizeColor(df,colorVar,sizeVar,latVar,lonVar,sizeThresh1,sizeThresh2,
#'              xmin,xmax,ymin,ymax,xleft=xleft,xright=xright,ytop=ytop,
#'              ybottom=ybottom,mainTitle=mainTitle,includeLabels=FALSE,
#'              LegCex=LegCex,titlePos=titlePos)
#'dev.off()
#'#To view the produced plot, us the following command:
#'\dontrun{shell.exec("GreatLakesExamplePlotNoLabels.pdf")}
#'
#'# With labels:
#'
#' df <- merge(df, SI, by=c("lat","lon"))
#' labelVar <- "Site"
#' offsetLatVar <- "offsetLat"
#' offsetLonVar <- "offsetLon"
#' offsetLineLatVar <- "offsetLineLat"
#' offsetLineLonVar <- "offsetLineLon" 
#' 
#' #Example works best in a landscape view:
#' pdf("GreatLakesExamplePlot.pdf",width=11,height=8)
#' MapSizeColor(df,colorVar,sizeVar,latVar,lonVar,sizeThresh1,sizeThresh2,
#'              xmin,xmax,ymin,ymax,xleft=xleft,xright=xright,ytop=ytop,ybottom=ybottom,mainTitle=mainTitle,includeLabels=TRUE,
#'              labels=labelVar, offsetLat=offsetLatVar, offsetLon=offsetLonVar,offsetLineLat=offsetLineLatVar,
#'              offsetLineLon=offsetLineLonVar,LegCex=LegCex,titlePos=titlePos)
#'dev.off()
#'#To view the produced plot, us the following command:
#'\dontrun{shell.exec("GreatLakesExamplePlot.pdf")}
MapSizeColor <- function(df,colorVar,sizeVar,latVar,lonVar,
                         sizeThresh1,sizeThresh2,
                         xmin,xmax,ymin,ymax,
                         col1="tan",col2="orange3",col3="orangered1",col4="orangered4",
                         xleft,xright,ytop,ybottom,mainTitle="",units=2,includeLabels,
                         labels="",offsetLat="",offsetLon="",offsetLineLat="",offsetLineLon="",LegCex=0.9, titlePos=-4){
  
  #set plot parameters
  par( mar=c(0,0,1,0), new = FALSE,xpd=NA)#,mgp=c(3,0.1,0))
  
  #Choose plot color bins: 
  #Use 0.25, 0.5, and 0.75 quantiles of non-zero values to define bins
  which(df[,colorVar] != 0)
  binThresh <- quantile(df[which(df[,colorVar] != 0),colorVar],c(0.25,0.5,0.75))
  binCol <- c(col2,col3,col4)
  plotSymbol <- 21 
  plotSize <- ifelse(df[,sizeVar] < sizeThresh1,1,1.5)
  plotSize <- ifelse(df[,sizeVar] > sizeThresh2,2,plotSize)
  
  fillCol <- rep(col1,dim(df)[1])
  
  for (i in 1:length(binThresh)) fillCol <- ifelse(df[,colorVar] > binThresh[i],binCol[i],fillCol)

  retList <- clipShape(xmin,xmax,ymin,ymax)
  plotBackgroundMap(retList)
  
  if(includeLabels){
    MapLabels(df=df,labels=labels,dataLat=latVar,dataLon=lonVar,
              offsetLat=offsetLat,offsetLon=offsetLon,
              offsetLineLat=offsetLineLat,
              offsetLineLon=offsetLineLon,
              cex=0.75)
  }
  
  points(df[,lonVar], df[,latVar],pch=plotSymbol, col="black",cex=plotSize,bg=fillCol)
  mtext(mainTitle,side=3,line=titlePos,outer=TRUE,font=2,cex=1.3)
  
  legendTextCex <- LegCex
  rect(xleft=xleft,ybottom=ybottom,xright=xright,ytop=ytop,col="white",)
  legend(x=xleft+0.2,y=ytop-0.9,c(paste("1-",sizeThresh1," samples",sep=""),
                                  paste((sizeThresh1+1),"-",sizeThresh2," samples",sep=""),
                                  paste("> ",sizeThresh2," samples",sep="")),bty="n",
         #       title=expression(bold("Number of Samples")),
         pch=c(21),pt.cex=c(1,1.5,2),bg="white",pt.bg="orange3",cex=LegCex+.1)
  binThresh <- round(binThresh,3)

  legendText= c(paste("<",binThresh[1]),
                paste(binThresh[1],"-",binThresh[2]),
                paste(binThresh[2],"-",binThresh[3]),
                paste(binThresh[3],"-",round(max(df[,colorVar],na.rm=TRUE),3)))
  legendCol = binCol
  startText <- c((xleft+xright)/2,ytop-0.3)
  text("Size of symbol",x=startText[1],y=startText[2],font=2,cex=legendTextCex)
  text("indicates number",x=startText[1],y=startText[2]-0.3,font=2,cex=legendTextCex)
  text("of samples",x=startText[1],y=startText[2]-0.6,font=2,cex=legendTextCex)
  
  startText <- c((xleft+xright)/2,ytop-2.4)
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
  
  legend(x=xleft+0.2,y=ytop-3.,legendText,pt.bg=c("tan",binCol),pch=plotSymbol,bg="white",
         cex=legendTextCex, pt.cex=legendTextCex,bty="n")
}

