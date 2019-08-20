# MazamaCoreUtils 0.3.1

* Tweak for CRAN.
* Renamed `lintFunctionArgs_directory()` to `lintFunctionArgs_dir()`.
* Added `fullPath` argument to linting funcitons.
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
