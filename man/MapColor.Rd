\name{MapColor}
\alias{MapColor}
\title{Mapping data with variable color to define spatial data}
\usage{
MapColor(df, colorVar, latVar, lonVar, politicalBounds, hydroPolygons,
  hydroLines, xmin, xmax, ymin, ymax, col1 = "tan", col2 = "orange3",
  col3 = "orangered1", col4 = "orangered4", xleft, xright, ytop, ybottom,
  mainTitle = "", units = 2)
}
\arguments{
  \item{df}{Dataframe with a column that has data to define
  symbol color, latitude, and longitude of points to be
  plotted}

  \item{colorVar}{Column name in df to define symbol color}

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

  \item{col1}{Symbol color for 0-0.25 quantile bin}

  \item{col2}{Symbol color for 0.25-0.5 quantile bin}

  \item{col3}{Symbol color for 0.5-0.75 quantile bin}

  \item{col4}{Symbol color for 0.75-1.0 quantile bin}

  \item{xleft}{Placement of left side of legend box (min
  latitude)}

  \item{xright}{Placement of right side of legend box (min
  latitude)}

  \item{ybottom}{Placement of bottom side of legend box
  (min longitude)}

  \item{ytop}{Placement of top side of legend box (min
  longitude)}

  \item{mainTitle}{Text to be used as the title of the
  plot}

  \item{units}{1-4 are for water concentration: mg/L, ug/L,
  ng/L, pg/L respectively. 5-6 are for sediment
  concentration: mg/kg, ug/kg, ng/kg, pg/kg respectively.}
}
\description{
Mapping routine that displays spatial data variability by
color differences. over layers with political boundaries,
hydrologic polygons, and hydrologic lines.
}
\examples{
lat.dd <- stationINFO$dec.lat.va
lon.dd <-  stationINFO$dec.long.va
y <- runif(n=length(lat.dd),min=0,max=50)
df <- data.frame(y=y, lat.dd=lat.dd,lon.dd=lon.dd)
colorVar <- "y"
latVar <- "lat.dd"
lonVar <- "lon.dd"
politicalBounds <- shape_poliboundsClip
hydroPolygons <- subShape_hydropolyClip
hydroLines <- shape_hydrolineClip
xmin <- -96.5
xmax <- -72
ymin <- 40.5
ymax <- 49.5
xleft <- -95
ybottom <- 40.7
xright <- -90.8
ytop <- 43.5
col1 <- "tan"
col2 <- "orange3"
col3 <- "orangered1"
col4 <- "orangered4"
mainTitle <- "OC Pesticides"
MapColor(df,colorVar,latVar,lonVar,
         politicalBounds,hydroPolygons,hydroLines,
         xmin,xmax,ymin,ymax,xleft=xleft,xright=xright,ytop=ytop,ybottom=ybottom,mainTitle=mainTitle)
}
\keyword{color}
\keyword{map}
\keyword{spatial}

