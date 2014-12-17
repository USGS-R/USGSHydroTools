#' Mapping data with variable size and color to define spatial data
#' 
#' Mapping routine that displays spatial data variability by size and color differences.
#' over layers with political boundaries, hydrologic polygons, and hydrologic lines.
#' 
#' @param df Dataframe with 4 required columns that have data to define symbol size, color,
#' latitude, and longitude of points to be plotted, and optionally 5 addtional columns: label names, 
#' offset for latitude and longitude for labels, and offset of latitude and longitude for the ending
#' point of lines pointing to the labels if needed
#' @param colorVar string, column name in df to define symbol color. If NA, all points colored the same, and no color legend is shown.
#' @param sizeVar string, column name in df to define symbol size. If NA, all points are the same size, and no size legend is shown.
#' @param latVar string, column name in df to define latitude
#' @param lonVar string, column name in df to define longitude
#' @param xmin numeric, left longitudinal boundary for plotting
#' @param xmax numeric, right longitudinal boundary for plotting
#' @param ymin numeric , bottom latitudinal boundary for plotting
#' @param ymax numeric, top latitudinal boundary for plotting
#' @param colVector vector of colors. Should be one more than the length of colThresh.
#' @param xleft numeric, placement of left side of legend box (min latitude)
#' @param ytop  numeric, placement of top side of legend box (min longitude)
#' @param mainTitle string, text to be used as the title of the plot
#' @param colText string or expression, text to label the color legend box
#' @param sizeText string or expression, text to label the size legend box
#' @param sizeThresh vector of values used to determine size bins.
#' @param colThresh vector of values used to determine color bins.
#' @param includeLabels logical, if TRUE labels will be included on plot. Defaults to FALSE.
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
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' (for example, adjusting margins with par(mar=c(5,5,5,5))).
#' @param colBinText string vector used to create legend text for color distributions. If NA (default), the 
#' function will create the text based on colThresh
#' @param sizeBinText string vector used to create legend text for size distributions. If NA (default), the 
#' function will create the text based on sizeThresh
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
#' sizeThresh <- c(sizeThresh1, sizeThresh2)
#' colThresh <- quantile(df[which(df[,colorVar] != 0),colorVar],c(0.25,0.5,0.75))
#' colThresh <- round(colThresh, digits=2)
#' LegCex <- 0.9
#' mainTitle <- "Colors vary by concentration"
#' titlePos <- -2
#' colorText <- expression(bold(atop("Colors represent","concentration in ["*mu*"g/L]")))
#' 
#' 
#'# Without labels:
#' 
#' #Example works best in a landscape view:
#' #pdf("GreatLakesExamplePlotNoLabels.pdf",width=11,height=8)
#' MapSizeColor(df,colorVar,sizeVar,latVar,lonVar,
#'              sizeThresh,colThresh,
#'              xmin,xmax,ymin,ymax,
#'              xleft,ytop,
#'              mainTitle=mainTitle,
#'              LegCex=LegCex,titlePos=titlePos)
#' #dev.off()
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
#' MapSizeColor(df,colorVar,sizeVar,latVar,lonVar,
#'              sizeThresh,colThresh,
#'              xmin,xmax,ymin,ymax,
#'              xleft,ytop,
#'              mainTitle=mainTitle,includeLabels=TRUE,
#'              labels=labelVar, offsetLat=offsetLatVar, 
#'              offsetLon=offsetLonVar,offsetLineLat=offsetLineLatVar,
#'              offsetLineLon=offsetLineLonVar,LegCex=LegCex,titlePos=titlePos)
#'dev.off()
#'#To view the produced plot on a Windows machine, us the following command:
#'\dontrun{shell.exec("GreatLakesExamplePlot.pdf")}
#'#To view the produced plot on a Mac, us the following command:
#'\dontrun{system("open GreatLakesExamplePlot.pdf")}
MapSizeColor <- function(df,colorVar,sizeVar,latVar,lonVar,
                         sizeThresh,colThresh,
                         xmin,xmax,ymin,ymax,
                         xleft,ytop,
                         includeLabels=FALSE,
                         colVector=c("tan","orange3","orangered1","orangered4"),
                         mainTitle="",
                         labels="",sizeText="number of samples",colText="Concentration",
                         offsetLat="",offsetLon="",offsetLineLat="",offsetLineLon="",
                         LegCex=0.9, titlePos=-4,customPar=FALSE,
                         colBinText=NA,sizeBinText=NA){
  
  #set plot parameters
  if(!customPar) par( mar=c(0,0,1,0), new = FALSE,xpd=NA)#,mgp=c(3,0.1,0))
    
  if(!is.na(colorVar)){
    
    if(all(is.na(colBinText))){
      if(is.numeric(df[,colorVar])){
        legendText <- c(paste("<=",colThresh[1])) 
        for(i in 2:length(colThresh)-1){
          legendText <- c(legendText, paste(colThresh[i],"-",colThresh[i+1]))
        }
        legendText <- c(legendText,paste(">",colThresh[length(colThresh)]))
      } else if(is.character(df[,colorVar])){
        factorData <- factor(df[,colorVar])
        legendText <- levels(factorData)
      } else if(is.factor(df[,colorVar])){
        legendText <- levels(df[,colorVar])
      } else {
        message("Check type")
      }
    } else {
      legendText <- colBinText
    }
    
    if(is.factor(df[,colorVar])){
      valToBin <- as.integer(df[,colorVar])
      colThresh <- 1:length(levels(df[,colorVar]))
      colThresh <- c(-Inf,colThresh)
    } else if (is.numeric(df[,colorVar])){
      colThresh <- c(-Inf,colThresh,Inf)
      valToBin <- df[,colorVar]
      
    } else if (is.character(df[,colorVar])){
      colFactor <- as.factor(df[,colorVar])   
      valToBin <- as.integer(colFactor)
      legendText <- levels(colFactor)
      colThresh <- 1:length(levels(colFactor))
      colThresh <- c(-Inf,colThresh)
      if(any(legendText != colBinText) & !all(is.na(colBinText))){
        message("Color legend corresponds to levels associated with colVar, igoring colBinText request")
      }
      
    } else {
#       message("Check type")
      # more conditions?
      valToBin <- df[,colorVar]
    }

    
    bins <- cut(valToBin,colThresh)
    colTransform <- setNames(colVector,levels(bins))
    df$fillCol <- colTransform[bins]
    
    #Reorder dataframe so unique colors are last (and on top of map)
#     df <- df[order(bins,decreasing=TRUE),]
        
  }
  
  plotSize <- rep(1,nrow(df))
  if(!is.na(sizeVar)){    
    
    if(all(is.na(sizeBinText))){
      if(is.numeric(df[,sizeVar])){
        legSizeText <- c(paste("<=",sizeThresh[1])) 
        for(i in 2:length(sizeThresh)-1){
          legSizeText <- c(legSizeText, paste(sizeThresh[i],"-",sizeThresh[i+1]))
        }
        legSizeText <- c(legSizeText,paste(">",sizeThresh[length(sizeThresh)]))
      } else if(is.character(df[,sizeVar])){
        factorData <- factor(df[,sizeVar])
        legSizeText <- levels(factorData)
      } else if(is.factor(df[,sizeVar])){
        legSizeText <- levels(df[,sizeVar])
      } else {
        message("Check type")
      }
    } else {
      legSizeText <- sizeBinText
    }
    
    if(is.factor(df[,sizeVar])){
      valToBinSize <- as.integer(df[,sizeVar])
      sizeThresh <- 1:length(levels(df[,sizeVar]))
      sizeThresh <- c(-Inf,sizeThresh)
    } else if (is.numeric(df[,sizeVar])){
      valToBinSize <- df[,sizeVar]
      sizeThresh <- c(-Inf,sizeThresh,Inf)
    } else if (is.character(df[,sizeVar])){
      sizeFactor <- as.factor(df[,sizeVar])
      valToBinSize <- as.integer(sizeFactor)
      sizeThresh <- 1:length(levels(sizeFactor))
      sizeThresh <- c(-Inf,sizeThresh)
      
      legSizeText <- levels(sizeFactor)
      if(any(legSizeText != sizeBinText) & !all(is.na(sizeBinText))){
        message("Size legend corresponds to levels associated with sizeVar, igoring sizeBinText request")
      }
    } else {
      message("Check type")
      # more conditions?
      valToBinSize <- df[,sizeVar]
    }
     
    plotSize <- seq(1,2,length=length(sizeThresh)+1)[-1]
    binsSize <- cut(valToBinSize,sizeThresh)
    sizeTransform <- setNames(plotSize,levels(binsSize))
    df$plotSize <- sizeTransform[binsSize]
    
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
  
  points(df[,lonVar], df[,latVar],pch=plotSymbol, col="black",cex=df$plotSize,bg=df$fillCol)
  mtext(mainTitle,side=3,line=titlePos,outer=TRUE,font=2,cex=1.3)
  
  plotRegion <- par("usr")
  plotHeight <- plotRegion[4] - plotRegion[3]

  legendTextCex <- LegCex

  # How many lines over 1 for size title?
  titleLines <- ifelse(length(grep("\n",sizeText)) == 0, 0, grep("\n",sizeText))
  titleLines <- titleLines + ifelse(length(grep("atop",sizeText)) == 0, 0, grep("atop",sizeText))
  
  # How many lines over 1 for color title?
  titleColLines <- ifelse(length(grep("\n",colText)) == 0, 0, grep("\n",colText))
  titleColLines <- titleColLines + ifelse(length(grep("atop",colText)) == 0, 0, grep("atop",colText))
  
  if(!is.na(sizeVar)){
    leg1 <- legend(x=xleft,y=ytop,legSizeText,
           title=sizeText,
           pch=c(21),pt.cex=sizeTransform,pt.bg=colVector[1],cex=LegCex,bty="n")
  } else {
    leg1 <- list()
    leg1$rect[["top"]] <- ytop
    leg1$rect[["h"]] <- 0
    leg1$rect[["left"]] <- xleft
    leg1$rect[["w"]] <- 0
  }
  
  if(!is.na(colorVar)){
    
    leg2 <- legend(x=xleft,y=(leg1$rect[["top"]] - leg1$rect[["h"]])-titleColLines*.05*(plotHeight),
                   legendText,pt.bg=colVector,pch=plotSymbol,bg="white",
                   cex=LegCex, pt.cex=1.5, title=colText,bty="n") #pt.cex=legendTextCex
    
  } else {
    leg2 <- list()
    leg2$rect[["top"]] <- leg1$rect[["top"]]
    leg2$rect[["h"]] <- leg1$rect[["h"]]
    leg2$rect[["left"]] <- leg1$rect[["left"]]
    leg2$rect[["w"]] <- leg1$rect[["w"]] 
  }

  legTop <- leg1$rect[["top"]]+titleLines*.05*(plotHeight)
  legBottom <- leg2$rect[["top"]] - leg2$rect[["h"]] 
  legLeft <- min(leg2$rect[["left"]],leg1$rect[["left"]])
  legRight <- max(c(leg2$rect[["left"]] + leg2$rect[["w"]],leg1$rect[["left"]] + leg1$rect[["w"]]))
  
  rect(legLeft,  legBottom,  legRight, legTop, col="white" ) 

  if(!is.na(colorVar)){
    leg2 <- legend(x=xleft,y=(leg1$rect[["top"]] - leg1$rect[["h"]])-titleColLines*.05*(plotHeight),
                   legendText,pt.bg=colVector,pch=plotSymbol,
                   cex=LegCex, pt.cex=c(rep(1.5,length(colVector))), title=colText,bty="n")
  }
  
  if(!is.na(sizeVar)){
    leg1 <- legend(x=xleft,y=ytop,legSizeText,
                   title=sizeText,
                   pch=c(21),pt.cex=c(1,1.5,2),pt.bg=colVector[1],cex=LegCex,bty="n")
  }

}

