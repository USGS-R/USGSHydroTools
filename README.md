USGSHydroTools
============

Installation
------------

See [https://github.com/USGS-R/dataRetrieval](https://github.com/USGS-R/dataRetrieval) for information on the dependent package dataRetrieval

Windows user can install the package dependencies using the following command:
```r
install.packagtes(c("rgdal","rgeos","sp","dataRetrieval","raster"))
```

Mac users will have to work a little harder to install the dependent packages rgdal and rgeos. The latest instructions are:
```r
setRepositories(ind = c(1,6))
install.packages(c("rgdal","rgeos","sp","dataRetrieval","raster"))
```	

Once those are installed, use the package `devtools` to install via gitHub:
```r
install.packages("smwrBase", repo="http://usgs-r.github.com")
library(devtools)
install_github("USGS-R/USGSHydroTools")
```

Disclaimer
----------
This software is in the public domain because it contains materials that originally came from the United States Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

[
    ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](http://creativecommons.org/publicdomain/zero/1.0/)
