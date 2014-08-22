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
#' @param colVector vector of colors
#' @param xleft Placement of left side of legend box (min latitude)
#' @param ytop  Placement of top side of legend box (min longitude)
#' @param mainTitle Text to be used as the title of the plot
#' @param colText
#' @param sizeText
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
#' symbols representing number of samples per site. Default is 0.9
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
#'              xmin,xmax,ymin,ymax,xleft=xleft,ytop=ytop,
#'              mainTitle=mainTitle,includeLabels=FALSE,
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
#'              xmin,xmax,ymin,ymax,xleft=xleft,ytop=ytop,mainTitle=mainTitle,includeLabels=TRUE,
#'              labels=labelVar, offsetLat=offsetLatVar, offsetLon=offsetLonVar,offsetLineLat=offsetLineLatVar,
#'              offsetLineLon=offsetLineLonVar,LegCex=LegCex,titlePos=titlePos)
#'dev.off()
#'#To view the produced plot, us the following command:
#'\dontrun{shell.exec("GreatLakesExamplePlot.pdf")}
MapSizeColor <- function(df,colorVar,sizeVar,latVar,lonVar,
                         sizeThresh,colThresh,
                         xmin,xmax,ymin,ymax,
                         colVector=c("tan","orange3","orangered1","orangered4"),
                         xleft,ytop,mainTitle="",includeLabels,
                         labels="",sizeText="number of samples",colText="Concentration",
                         offsetLat="",offsetLon="",offsetLineLat="",offsetLineLon="",
                         LegCex=0.9, titlePos=-4){
  
  #set plot parameters
  par( mar=c(0,0,1,0), new = FALSE,xpd=NA)#,mgp=c(3,0.1,0))
  
  #Choose plot color bins: 
  #Use 0.25, 0.5, and 0.75 quantiles of non-zero values to define bins
  
  fillCol <- rep(colVector[1],nrow(df))
  
  if(!is.na(colorVar)){
    binCol <- colVector[-1]
    
    for (i in 1:length(colThresh)) fillCol <- ifelse(df[,colorVar] > colThresh[i],binCol[i],fillCol)
   
    legendText <- c(paste("<",colThresh[1])) 
    for(i in 2:length(colThresh)-1){
      legendText <- c(legendText, paste(colThresh[i],"-",colThresh[i+1]))
    }
    legendText <- c(legendText,paste(">",colThresh[length(colThresh)]))
    
  }
  
  plotSize <- rep(1,nrow(df))
  if(!is.na(sizeVar)){
    
    binSize <- seq(1,2,length=length(sizeThresh)+1)[-1]
    for (i in 1:length(sizeThresh)) plotSize <- ifelse(df[,sizeVar] > sizeThresh[i],binSize[i],plotSize)

    legSizeText <- c(paste("<",sizeThresh[1])) 
    for(i in 2:length(sizeThresh)-1){
      legSizeText <- c(legSizeText, paste(sizeThresh[i],"-",sizeThresh[i+1]))
    }
    legSizeText <- c(legSizeText,paste(">",sizeThresh[length(sizeThresh)]))
  } else {
    plotSize <- 1
  }
  
  plotSymbol <- 21 
  
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
  
  plotRegion <- par("usr")
  plotHeight <- plotRegion[4] - plotRegion[3]

  legendTextCex <- LegCex

  if(!is.na(sizeVar)){
    leg1 <- legend(x=xleft,y=ytop,legSizeText,
           title=sizeText,
           pch=c(21),pt.cex=c(1,1.5,2),pt.bg=colVector[2],cex=LegCex,bty="n")
  } else {
    leg1 <- list()
    leg1$rect[["top"]] <- ytop
    leg1$rect[["h"]] <- 0
    leg1$rect[["left"]] <- xleft
    leg1$rect[["w"]] <- 0
  }
  
  if(!is.na(colorVar)){
    
    leg2 <- legend(x=xleft,y=(leg1$rect[["top"]] - leg1$rect[["h"]])-.05*(plotHeight),
                   legendText,pt.bg=c("tan",binCol),pch=plotSymbol,bg="white",
                   cex=LegCex, pt.cex=1.5, title=colText,bty="n") #pt.cex=legendTextCex
    
  } else {
    leg2 <- list()
    leg2$rect[["top"]] <- leg1$rect[["top"]]
    leg2$rect[["h"]] <- leg1$rect[["h"]]
    leg2$rect[["left"]] <- leg1$rect[["left"]]
    leg2$rect[["w"]] <- leg1$rect[["w"]] 
  }

  
  legTop <- leg1$rect[["top"]]+.05*(plotHeight)
  legBottom <- leg2$rect[["top"]] - leg2$rect[["h"]] 
  legLeft <- min(leg2$rect[["left"]],leg1$rect[["left"]])
  legRight <- max(c(leg2$rect[["left"]] + leg2$rect[["w"]],leg1$rect[["left"]] + leg1$rect[["w"]]))
  
  rect(legLeft,  legBottom,  legRight, legTop, col="white" ) 

  if(!is.na(colorVar)){
    leg2 <- legend(x=xleft,y=(leg1$rect[["top"]] - leg1$rect[["h"]])-.05*(plotHeight) ,legendText,pt.bg=c("tan",binCol),pch=plotSymbol,
                   cex=LegCex, pt.cex=c(rep(1.5,length(binCol)+1)), title=colText,bty="n")
  }
  
  if(!is.na(sizeVar)){
    leg1 <- legend(x=xleft,y=ytop,legSizeText,
                   title=sizeText,
                   pch=c(21),pt.cex=c(1,1.5,2),pt.bg="orange3",cex=LegCex,bty="n")
  }

}

