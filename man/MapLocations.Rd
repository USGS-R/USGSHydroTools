\name{MapLocations}
\alias{MapLocations}
\title{Mapping locations of interst}
\usage{
MapLocations(df, latVar, lonVar, politicalBounds, hydroPolygons, hydroLines,
  xmin, xmax, ymin, ymax, col1 = "tan", mainTitle = "", includeLabels,
  labels = "", offsetLat = "", offsetLon = "", offsetLineLat = "",
  offsetLineLon = "")
}
\arguments{
  \item{df}{Dataframe with 3 required columns to define
  symbol color, latitude, and longitude of points to be
  plotted, and optionally 5 addtional columns: label names,
  offset for latitude and longitude for labels, and offset
  of latitude and longitude for the ending}

  \item{latVar}{Column name in df to define latitude}

  \item{lonVar}{Column name in df to define longitude}

  \item{politicalBounds}{Shapefile of class
  "SpatialPolygonsDataFrame" for defining political
  boundaries}

  \item{hydroPolygons}{Shapefile of class
  "SpatialPolygonsDataFrame" for defining hydrologic
  polygons (lakes)}

  \item{hydroLines}{shapefile of class
  "SpatialLinesDataFrame" for defining hydrologic lines
  (rivers/streams)}

  \item{xmin}{Left longitudinal boundary for plotting}

  \item{xmax}{Right longitudinal boundary for plotting}

  \item{ymin}{Bottom latitudinal boundary for plotting}

  \item{ymax}{Top latitudinal boundary for plotting}

  \item{col1}{Symbol color for site location}

  \item{mainTitle}{Text to be used as the title of the
  plot}

  \item{includeLabels}{logical, if TRUE labels will be
  included on plot.}

  \item{labels}{String variable in dataframe df with label
  names}

  \item{offsetLat}{Variable in dataframe df for the offset
  from dataLat used for label positioning}

  \item{offsetLon}{Variable in dataframe df for the offset
  from dataLon used for label positioning}

  \item{offsetLineLat}{Variable in dataframe df for the
  offset from dataLat used to position the end of the line
  drawn to the label. Lines are optional.}

  \item{offsetLineLon}{Variable in dataframe df for the
  offset from dataLon used to position the end of the line
  drawn to the label. Lines are optional.}
}
\description{
Mapping routine that displays locations of sites over
layers with political boundaries, hydrologic polygons, and
hydrologic lines.
}
\examples{
lat <- SI$lat
lon <-  SI$lon
df <- data.frame(lat=lat,lon=lon)
latVar <- "lat"
lonVar <- "lon"

politicalBounds <- shape_poliboundsClip
hydroPolygons <- subShape_hydropolyClip
hydroLines <- shape_hydrolineClip
xmin <- -96.5
xmax <- -72
ymin <- 40.5
ymax <- 49.5
mainTitle <- "Site Locations"

#Without labels

#Example works best in a landscape view:
pdf("GreatLakesExamplePlotNoLabels.pdf",width=11,height=8)
MapLocations(df,latVar,lonVar,
             politicalBounds,hydroPolygons,hydroLines,
             xmin,xmax,ymin,ymax,mainTitle=mainTitle,
             includeLabels=FALSE)
dev.off()
#To view the produced plot, us the following command:
\dontrun{shell.exec("GreatLakesExamplePlotNoLabels.pdf")}

# With labels:
df <- merge(df, SI, by=c("lat","lon"))
labelVar <- "Site"
offsetLatVar <- "offsetLat"
offsetLonVar <- "offsetLon"
offsetLineLatVar <- "offsetLineLat"
offsetLineLonVar <- "offsetLineLon"
#Example works best in a landscape view:
pdf("GreatLakesExamplePlot.pdf",width=11,height=8)
MapLocations(df,latVar,lonVar,
             politicalBounds,hydroPolygons,hydroLines,
             xmin,xmax,ymin,ymax,mainTitle=mainTitle,includeLabels=TRUE,
             labels=labelVar, offsetLat=offsetLatVar, offsetLon=offsetLonVar,offsetLineLat=offsetLineLatVar,
             offsetLineLon=offsetLineLonVar)
dev.off()
#To view the produced plot, us the following command:
\dontrun{shell.exec("GreatLakesExamplePlot.pdf")}
}
\keyword{color}
\keyword{map}
\keyword{spatial}

