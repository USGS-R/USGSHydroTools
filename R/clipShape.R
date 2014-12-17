#' Clip supplied shapefiles
#' 
#' Clips the political boundary, hydrolines, and hydroshape shapefiles to user-defined lat/lon limits.
#' 
#' @param xmin numeric minimum latitude
#' @param xmax numeric maximum latitude
#' @param ymin numeric minimum longitude
#' @param ymax numeric maximum longitude
#' @return list of politicalBoundary, rivers, and lakes shapefiles, and a vector of lat/lon limits
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
#' polShape <- retList$politicalBoundary
#' riverShape <- retList$rivers
#' lakeShape <- retList$lakes
clipShape <- function(xmin,xmax,ymin,ymax){
  
  shape_hydropoly <- shape_hydropoly
  shape_polibounds <- shape_polibounds
  shape_hydroline <- shape_hydroline
  
  ext <- extent(xmin,xmax,ymin,ymax) 
  clipe <- as(ext, "SpatialPolygons") 
  proj4string(clipe) <- CRS(proj4string(shape_polibounds)) 
  cropd <- SpatialPolygonsDataFrame(clipe, data.frame(x = 1), match.ID = FALSE) 
  shape_poliboundsClipped <- gIntersection(shape_polibounds, cropd,byid=TRUE) 

  proj4string(clipe) <- CRS(proj4string(shape_hydropoly)) 
  cropd <- SpatialPolygonsDataFrame(clipe, data.frame(x = 1), match.ID = FALSE) 
  shape_hydropolyClipped <- gIntersection(shape_hydropoly, cropd,byid=TRUE) 
  
  proj4string(clipe) <- CRS(proj4string(shape_hydroline)) 
  cropd <- SpatialPolygonsDataFrame(clipe, data.frame(x = 1), match.ID = FALSE) 
  shape_hydrolineClipped <- gIntersection(shape_hydroline, cropd,byid=TRUE) 
  
  return(list(politicalBoundary=shape_poliboundsClipped,
              lakes=shape_hydropolyClipped,
              rivers=shape_hydrolineClipped,
              limits=c(xmin,xmax,ymin,ymax)))
  
}