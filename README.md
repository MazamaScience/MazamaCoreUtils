# MazamaCoreUtils

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/MazamaCoreUtils)](https://cran.r-project.org/package=MazamaCoreUtils)
[![Downloads](https://cranlogs.r-pkg.org/badges/MazamaCoreUtils)](https://cran.r-project.org/package=MazamaCoreUtils)
[![DOI](https://zenodo.org/badge/152321630.svg)](https://zenodo.org/badge/latestdoi/152321630)

MazamaCoreUtils is a suite of utility functions for production-level R code,
providing functionality commonly needed by operational systems such as logging,
error handling, cache management and date-time parsing.

Functions for date-time parsing and formatting require that time zones be
specified explicitly, avoiding a common source of error when working with
environmental time series.

## Installation

Install the released version from CRAN:

```r
install.packages("MazamaCoreUtils")
```

Install the development version from GitHub:

```r
remotes::install_github("MazamaScience/MazamaCoreUtils")
```

MazamaCoreUtils requires R version 4.0.0 or later.

## Example

Set up standard log files, request a range of days in an explicit time zone and
record the request in the log:

```r
library(MazamaCoreUtils)

# Create TRACE, DEBUG, INFO, WARN and ERROR log files
logDir <- file.path(tempdir(), "logs")
initializeLogging(logDir)

# Date-times always require an explicit time zone
tlim <- dateRange("2024-03-01", "2024-03-03", timezone = "America/Los_Angeles")
tlim
#> [1] "2024-03-01 00:00:00 PST" "2024-03-02 23:59:59 PST"

logger.info(
  "Requesting data from %s to %s",
  timeStamp(tlim[1], timezone = "America/Los_Angeles", unit = "min", style = "clock"),
  timeStamp(tlim[2], timezone = "America/Los_Angeles", unit = "min", style = "clock")
)

readLines(file.path(logDir, "INFO.log"))
```

## Overview

- **Logging** — `initializeLogging()`, `logger.setup()`, `logger.setLevel()`,
  `logger.info()`, `logger.error()`
- **Error handling and `NULL` values** — `stopOnError()`, `stopIfNull()`,
  `setIfNull()`
- **Date-times with explicit time zones** — `parseDatetime()`, `dateRange()`,
  `timeRange()`, `dateSequence()`, `timeStamp()`
- **Locations** — `validateLonLat()`, `createLocationMask()`,
  `createLocationID()`
- **Cache management and data loading** — `manageCache()`, `loadDataFile()`
- **API keys** — `setAPIKey()`, `getAPIKey()`, `showAPIKeys()`
- **HTML utilities** — `html_getLinks()`, `html_getTables()`
- **Code linting and package checks** — `lintFunctionArgs_file()`,
  `timezoneLintRules`, `check_fast()`

Detailed argument and return value documentation is available in the function
help pages, for example `?parseDatetime`.

## Background

MazamaCoreUtils was created by Mazama Science to standardize the work of
building R packages, data processing pipelines and web services focused on
environmental monitoring data. Its functions are used by other Mazama Science
packages and by systems that are run operationally, where consistent logging,
clear error messages and unambiguous time zones matter.

## Documentation and Help

- [CRAN package page](https://cran.r-project.org/package=MazamaCoreUtils) for
  the reference manual and vignettes on cache management, date parsing, error
  handling and logging
- [GitHub Issues](https://github.com/MazamaScience/MazamaCoreUtils/issues) for bug reports
- [GitHub Discussions](https://github.com/MazamaScience/MazamaCoreUtils/discussions)
  for announcements, support and building a community of practice around this
  package

Questions regarding further development of the package can also be directed to
<jonathan.s.callahan@gmail.com>.

## Citation

For citation information, use:

```r
citation("MazamaCoreUtils")
```

## Acknowledgements

Development of this package has been supported with funding from the USFS
[AirFire Research Team](https://www.airfire.org).

## License

MazamaCoreUtils is released under the GPL-3 license.
