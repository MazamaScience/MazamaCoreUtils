# MazamaCoreUtils 0.4.6

* Updated `check_~()` functions to use `devtools::check()` `manual` and 
`run_dont_test` arguments.

# MazamaCoreUtils 0.4.5

* Added `header` option to `html_getLinks()` and `html_getTables()` functions.

# MazamaCoreUtils 0.4.4

* Added new `html_getLinks()` and `html_getTables()` functions.

# MazamaCoreUtils 0.4.3

* Now exporting `futile.logger::flog.layout()` to avoid downstream complaints.
* Updated documentation for `check_~()` functions.

# MazamaCoreUtils 0.4.2

* Fixed error messaging in `loadDataFile()`.
* Added `ceilingStart` argument to `dateRange()` and `timeRange()`.

# MazamaCoreUtils 0.4.1

* Fixed improper handling of single day date ranges in `dateRange()`.

# MazamaCoreUtils 0.4.0

This version address a few issues encountered while using 0.3.11 in production.

* New `timeStamp()` function creates character timestamps useful for labeling.
* New `dateSequence()` function generates a sequence of `POSIXct` values that 
align with midnight local time even through the switch to/from daylight
savings.
* New `loadDataFile()` function supports loading data from local directories or 
URLs.
* Added year-only support to `parseDatetime()` when parsing julian dates.
* Renamed `parsDatetime()` argument `julian` to `isJulian`.
* `stopIfNull()` and `setIfNull()` now issue `stop(...)` messages with 
`call. = TRUE` so that the stack information bubbles up.
* Now exporting `futile.logger::flog.logger()` to avoid downstream complaints.

# MazamaCoreUtils 0.3.11

* Added this package's own date parsing functions to `timezoneLintrules`.

# MazamaCoreUtils 0.3.10

* `parseDatetime()` now supports YYYY and YYYYmm formats.

# MazamaCoreUtils 0.3.9

* Added `julian` argument to `parseDatetime()` to support Julian day formats.

# MazamaCoreUtils 0.3.8

Added a suite of functions to easily run `devtools::check()` with different
arguments:
* `check_fastest()`
* `check_faster()`
* `check_fast()`
* `check()`
* `check_slow()`
* `check_slower()`
* `check_slowest()`

* Added more *lubridate* functions to `timezoneLintRules`.

# MazamaCoreUtils 0.3.7

* Added `logger.isInitialized()` for programmatic use.
* `logger.setLevel()` now guarantees that `logger.setup()` has been called,
fixing a bug that generated multiple output messages when `logger.setLevel()`
was called before `logger.setup()`.
* Added more *lubridate* functions to `timezoneLintRules`.

# MazamaCoreUtils 0.3.6

More consistency improvements to `dateRange()`. When specified, the `days` 
parameter now takes precedence over `ceilingEnd` when no `enddate` is specified.

# MazamaCoreUtils 0.3.5

Fully self-consistent package using internal functions wherever possible.

# MazamaCoreUtils 0.3.4

Various improvements after usage in an operational setting:

* `timezoneLintRules` includes more date related functions.
* Added `quiet` argument to `parseDateTime()`.
* `timeRange()` function now accepts `unit` and `ceilingEnd` arguments.
* `dateRange()` function now accepts `ceilingEnd` argument.
* More unit tests for dates and times.

The `ceilingEnd` argument addresses the ambiguity of a phrase like:
"August 1-8". With `ceilingEnd = FALSE` (default) this pharse means "through the
beginning of Aug 8". With `ceilingEnd = TRUE` it means "through the end of Aug 8".

# MazamaCoreUtils 0.3.3

This version adds new convenience functions for dealing with `NULL` values
in pipeline flow control.

## New functions

 * `setIfNull()`
 * `stopIfNull()`

## Dealing with NULL values

In a lot of the data pipelines we build (web services, packages, etc.), we need
to deal with the possibility of `NULL` inputs. Setting a default value or
throwing an error are two of the most common ways `NUll` values are dealt with.
While the code to handle this is straightforward to write, it is verbose and
repetitive. `setIfNull()` and `stopIfNull()` are designed to abstract away
boilerplate code, allowing us to focus on more important things.

# MazamaCoreUtils 0.3.2

* Improved documentation for `enddate` in `dateRange().

# MazamaCoreUtils 0.3.1

* Tweak for CRAN.
* Renamed `lintFunctionArgs_directory()` to `lintFunctionArgs_dir()`.
* Added `fullPath` argument to linting functions.
* Added `date-parsing` vignette.

# MazamaCoreUtils 0.3.0

This version focuses on enhancing the time utility capabilities of
_MazamaCoreUtils_.

## New functions

 * `lintFunctionArgs_file()`
 * `lintFunctionArgs_directory()`
 * `timeRange()`
 * `parseDatetime()`

## Function Argument Linting

In order to ensure that we are working with timezones consistently, the
functions `lintFunctionArgs_file()` and `lintFunctionArgs_directory()` were
created to parse **R** source files and determine if certain functions contained
specific named arguments, based on a set of rules. A set of rules,
`timezoneLintRules`, was created to check that all functions that accept a
timezone have that argument explicitly fill, avoiding inconsistent default
behavior.

## Consolidating Time-Utility functions

`PWFSLSmoke::parseDatetime()` was moved into _MazamaCoreUtils_ so more packages
can benefit from it without importing the rest of _PWFSLSmoke_. Also,
`timeRange()` was created to work as a function analogous to `dateRange()`,
parsing time ranges instead of finding date ranges.

As part of this consolidation, more unit tests were added to the package.

# MazamaCoreUtils 0.2.1

 * Reordered arguments to `dateRange()`.
 * `dateRange()` no longer provides a default timezone.
 * `dateRange()` no longer alters `POSIXct` inputs
 * `dateRange()` now has stricter argument checking

# MazamaCoreUtils 0.2.0

 * Moved `futile.logger` from `Depends` to `Imports`.
 * Version bump.

# MazamaCoreUtils 0.1.901

 * Add linter
 * Add `Rproj` file
 * Minor style refactor

# MazamaCoreUtils 0.1.900

 * Added `dateRange()` function.

# MazamaCoreUtils 0.1.3

 * Added _pkgdown_ documentation.
 * New vignettes for `error-handling` and `logging`.
 * Package now **Depends:** on futile.logger -- avoids `futile.logger not found`
 error messages.
 * Additional logging unit tests.

# MazamaCoreUtils 0.1.2

 * Added `maxFileAge` parameter to `cacheManagement()` to help with removal of
 out-of-data products.
 * New dependencies on *magrittr* and *rlang* packages.

# MazamaCoreUtils 0.1.1

 * Minor cleanup and documentation improvements.

# MazamaCoreUtils 0.1.0

 * Initial functionality.
