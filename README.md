[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/MazamaCoreUtils)](https://cran.r-project.org/package=MazamaCoreUtils)
[![Downloads](http://cranlogs.r-pkg.org/badges/MazamaCoreUtils)](https://cran.r-project.org/package=MazamaCoreUtils)
[![Build Status](https://travis-ci.org/MazamaScience/MazamaCoreUtils.svg?branch=master)](https://travis-ci.org/MazamaScience/MazamaCoreUtils)
[![DOI](https://zenodo.org/badge/152321630.svg)](https://zenodo.org/badge/latestdoi/152321630)

# MazamaCoreUtils

```
A suite of utility functions providing functionality commonly needed for 
production level projects such as logging, error handling, cache management and 
date-time parsing. Functions for date-time parsing and formatting require that 
timezones be specified explicitly, avoiding a common source of error when 
working with environmental time series.
```

## Background

The MazamaCoreUtils package was created by MazamaScience to regularize our
work building R packages and R-based web services focused on environmental
monitoring data.

The main goal of this package is to create an internally standardized set of
functions that we can use in various systems that are being run operationally. 
Areas of functionality supported by this package include:

 * python style logging
 * simple error messaging
 * cache management
 * API key handling
 * date-time parsing
 * lat/lon validation and uniqueID creation
 * source code linting
 
## Installation

Install from CRAN with:

```install.packages('MazamaCoreUtils')```

Install the latest version from GitHub with:

``` devtools::install_github('mazamascience/MazamaCoreUtils') ```

----

This project is supported by Mazama Science.

