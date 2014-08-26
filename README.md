USGSHydroTools
============

Installation
------------

See [https://github.com/USGS-R/dataRetrieval](https://github.com/USGS-R/dataRetrieval) for information on the dependent package dataRetrieval

Windows user can install the package and it's dependencies using the following command:

	install.packages(c("USGSHydroTools"),
		repos=c("http://usgs-r.github.com",
			"http://cran.us.r-project.org",
			""),
		dependencies=TRUE)
		
Mac users will have to work a little harder to install the dependent packages rgdal and rgeos. The latest instructions are:

	setRepositories(ind = c(1,6))
	install.packages(c("rgdal","rgeos"))
	install.packages(c("USGSHydroTools"),
		repos=c("http://usgs-r.github.com",
			"http://cran.us.r-project.org",
			""),
		dependencies=TRUE)	


Disclaimer
----------
This software is in the public domain because it contains materials that originally came from the United States Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

This software is provided "AS IS".
