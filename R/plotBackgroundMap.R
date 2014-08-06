#' Set up map with political, lake, and river boundaries.
#' 
#' Set up map with political, lake, and river boundaries.
#' 
#' @param shapeList list returned from clipShape function (includes 3 shapefiles and a vector of lat/lon limits)
#' @return list of politicalBoundary, rivers, and lakes shapefiles
#' @import rgdal
#' @importFrom raster extent
#' @import sp
#' @import rgeos
#' @export
#' @examples
#' xmin <- -88-11/60 #MMSD
#' xmax <- -87-48/60 #MMSD
#' ymin <- 42+51/60 #MMSD
#' ymax <- 43+20/60 #MMSD
#' retList <- clipShape(xmin,xmax,ymin,ymax)
#' plotBackgroundMap(retList)
plotBackgroundMap <- function(shapeList){
  
  xmin <- shapeList$limits[1]
  xmax <- shapeList$limits[2]
  ymin <- shapeList$limits[3]
  ymax <- shapeList$limits[4]    
  polShape <- shapeList$politicalBoundary
  riverShape <- shapeList$rivers
  lakeShape <- shapeList$lakes  
  plot(polShape,col="gray90",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
  plot(lakeShape,col="lightskyblue2",xlim=c(xmin,xmax),ylim=c(ymin,ymax),add=TRUE)#
  lines(riverShape,col="lightskyblue2",xlim=c(xmin,xmax),ylim=c(ymin,ymax))#
  plot(polShape,add=TRUE)
  
}
  