# USGSHydroTools

## Package Status


|Linux|Test Coverage| USGS Status |
|----------|------------|------------|
[![Coverage Status](https://coveralls.io/repos/github/USGS-R/USGSHydroTools/badge.svg?branch=master)](https://coveralls.io/github/USGS-R/USGSHydroTools?branch=master)|[![status](https://img.shields.io/badge/USGS-Research-blue.svg)](https://owi.usgs.gov/R/packages.html#research)|

### Reporting bugs

Please consider reporting bugs and asking questions on the Issues page:
[https://github.com/USGS-R/USGSHydroTools/issues](https://github.com/USGS-R/USGSHydroTools/issues)

### Code of Conduct

We want to encourage a warm, welcoming, and safe environment for contributing to this project. See the [code of conduct](https://github.com/USGS-R/USGSHydroTools/blob/master/CONDUCT.md) for more information.

## Installation

## Installation of R and RStudio

This section should only need to be done once per computer.

The following link walks you through an installation of R and RStudio:

[Installation Instructions](https://owi.usgs.gov/R/training-curriculum/intro-curriculum/Before/)

If you follow those instructions exactly, you should have the USGS R repository (GRAN) added to your R profile. If that step doesn't ring a bell, paste the following into your R console:

```r
rprofile_path = file.path(Sys.getenv("HOME"), ".Rprofile")
write('\noptions(repos=c(getOption(\'repos\'),
    CRAN=\'https://cloud.r-project.org\',
    USGS=\'https://owi.usgs.gov/R\'))\n',
      rprofile_path, 
      append =  TRUE)

cat('Your Rprofile has been updated to include GRAN.
    Please restart R for changes to take effect.')
```

*RESTART RSTUDIO!*

Useful links:

* [Download R Windows](https://cran.r-project.org/bin/windows/base/)
* [Download R Mac](https://cran.r-project.org/bin/macosx/)
* [Download RStudio](https://www.rstudio.com/products/rstudio/download/)



Mac users might have to work a little harder to install the dependent packages `rgdal` and `rgeos`. The latest instructions are:
```r
setRepositories(ind = c(1,6))
install.packages(c("rgdal","rgeos","sp","dataRetrieval","raster"))
```	

## Package Installation

Once the GRAN repository is added, you can install the package:

```
install.packages("USGSHydroTools")
```


## Disclaimer

This software is in the public domain because it contains materials that originally came from the United States Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

[
    ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](http://creativecommons.org/publicdomain/zero/1.0/)
