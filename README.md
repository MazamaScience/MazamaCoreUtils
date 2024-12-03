[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/MazamaCoreUtils)](https://cran.r-project.org/package=MazamaCoreUtils)
[![Downloads](https://cranlogs.r-pkg.org/badges/MazamaCoreUtils)](https://cran.r-project.org/package=MazamaCoreUtils)
[![DOI](https://zenodo.org/badge/152321630.svg)](https://zenodo.org/badge/latestdoi/152321630)

A dedicated Slack channel has been created for announcements, support and to 
help build a community of practice around this open source package. You may 
request an invitation to join from <jonathan.s.callahan@gmail.com>.

# MazamaCoreUtils

```
A suite of utility functions providing functionality commonly needed for 
production level projects such as logging, error handling, cache management and 
date-time parsing. Functions for date-time parsing and formatting require that 
timezones be specified explicitly, avoiding a common source of error when 
working with environmental time series.
```

## Background

The MazamaCoreUtils package was created by Mazama Science to regularize
work building R packages, data processing pipelines and web services focused on 
environmental monitoring data.

The main goal of this package is to create an internally standardized set of
functions that for use in various systems that are being run operationally. 
Areas of functionality supported by this package include:

 * python style logging
 * simple error messaging
 * cache management
 * API key handling
 * date-time parsing and formatting
 * lat/lon validation and uniqueID creation
 * source code linting
 
## Installation

Install from CRAN with:

```install.packages('MazamaCoreUtils')```

Install the latest version from GitHub with:

```devtools::install_github('mazamascience/MazamaCoreUtils')```

----

Development of this R package has been supported with funding from the 
following institutions:

* USFS [AirFire Research Team](https://www.airfire.org)

Questions regarding further development of the package should be directed to 
<jonathan.callahan@gmail.com>.

