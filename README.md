GSHydroTools
============

Disclaimer
----------
This software is in the public domain because it contains materials that originally came from the United States Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

This software is provided "AS IS".

Installation
------------

See [https://github.com/USGS-R/dataRetrieval](https://github.com/USGS-R/dataRetrieval) for information on the dependent package dataRetrieval

  install.packages(c("rgdal","sp","zoo","XML","RCurl","plyr","reshape2"),
          dependencies=TRUE)
	install.packages(c("dataRetrieval","USGSwsBase"), repo="http://usgs-r.github.com")
	install.packages("GSHydroTools", repo="http://usgs-r.github.com")
