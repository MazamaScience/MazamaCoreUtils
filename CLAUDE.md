# CLAUDE.md

AI-oriented project guide for `MazamaCoreUtils`. Read this first,
then consult the two companion documents as needed:

- **`.claude/CLAUDE_ARCHITECTURE.md`** — architecture, module map, call graph,
  public API contract, and design rationale.
- **`.claude/CLAUDE_STYLE_GUIDE.md`** — cross-project working principles
  (philosophy, refactoring, error handling, communication, review priorities).
  These are portable conventions shared across projects.

This file covers *conventions for this project*. The architecture doc
details *how this project works*; the style guide governs *how we write code*.


---

## Project Overview

`MazamaCoreUtils` is an R package of utility functions for production-level R
code, maintained by Mazama Science (version in `DESCRIPTION`). It is shared
infrastructure for other MazamaScience packages and for operational
data-processing pipelines and web services built around environmental
monitoring data.

- **Type:** library, published on CRAN and GitHub
  (`MazamaScience/MazamaCoreUtils`); documentation site built with pkgdown.
- **Consumers:** other MazamaScience R packages and internal systems. The
  exported API is therefore a public contract.
- **Goals:** reliability, correctness and simplicity. Fail loudly with clear
  messages rather than guess.
- **Maturity:** stable, maintenance-oriented. Recent work has been
  refactoring (logging moved to **logger** in 0.6.0), hardening edge cases and
  documentation.

Functional areas:

- Python-style logging (`logger.*()`, `initializeLogging()`)
- Error handling (`stopOnError()`, `stopIfNull()`, `setIfNull()`)
- Cache management (`manageCache()`) and data loading (`loadDataFile()`)
- API key handling (`setAPIKey()`, `getAPIKey()`, `showAPIKeys()`)
- Date-time parsing and formatting with *explicit timezones*
  (`parseDatetime()`, `dateRange()`, `dateSequence()`, `timeRange()`,
  `timeStamp()`)
- Longitude/latitude validation and location IDs (`validateLonLat()`,
  `validateLonsLats()`, `createLocationID()`, `createLocationMask()`)
- HTML scraping helpers (`html_getLinks()`, `html_getTables()`)
- Source-code linting for timezone arguments (`lintFunctionArgs_*()`,
  `timezoneLintRules`) and `devtools::check()` wrappers (`check_*()`)


---

## Language and Style

- **Language:** R (>= 4.0.0). Public functions carry **roxygen2** documentation
  (markdown enabled) with `@param`, `@return` and runnable `@examples`
  (`\dontrun{}` for error cases and network access).
- **Naming:** exported functions and arguments are camelCase (`parseDatetime`,
  `maxCacheSize`), with two legacy families, `logger.*()` and `html_*()`.
  Internal helpers start with `.`.
- **Code layout:** one argument per line in signatures; `# ----- Validate
  parameters ---` style section headers in function bodies. Follow the
  surrounding file.
- **Argument handling:** required arguments default to `NULL` and are checked
  with `stopIfNull()`; optional arguments are normalized with `setIfNull()`.
- **Namespacing:** `%>%` is re-exported from **magrittr**. Call other packages
  with `pkg::fun()`, and call this package's own exported utilities fully
  qualified too (`MazamaCoreUtils::stopIfNull()`); internal `.helpers` are
  called unqualified; `importFrom` is used only for **logger**, **magrittr**,
  **rlang** and `utils::str`.
- **Error messages:** `stop(sprintf("'arg = %s' ...", arg))`, naming the
  offending argument in single quotes.
- **Lint:** `.lintr` sets line length 100 and disables `object_name_linter`
  and `todo_comment_linter`.
- **Timezones:** never rely on the system timezone. Do not use `Sys.time()`,
  `Sys.Date()`, or timezone-less calls to `as.POSIXct()`, `lubridate::now()`,
  etc. (see `timezoneLintRules`).


---

## Project-Specific Constraints

### Public API and Compatibility

Everything in `NAMESPACE` is exported for downstream packages. Do not rename,
remove, or change the defaults, argument order, or return type of an exported
function without a deliberate version bump and a `NEWS.md` entry. Prefer
deprecation to removal (as with the `algorithm` argument of
`createLocationID()` in 0.6.2).

### Data Conventions

- Date-times are `POSIXct` with an explicit timezone, validated against
  `OlsonNames()`. Do not add defaults that silently fall back to local time.
- Longitudes are in [-180, 180] and latitudes in [-90, 90], in decimal degrees.
- Compact numeric/character datetimes such as `20181012130900` are a Mazama
  Science convention accepted by `parseDatetime()`.
- Invalid locations yield `invalidID` (default `NA`) in `createLocationID()`.

### Logging

The `logger.*()` API wraps **logger** in the fixed namespace `"MazamaCoreUtils"`
and preserves the legacy futile.logger-style interface, including the exported
level constants `FATAL`, `ERROR`, `WARN`, `INFO`, `DEBUG`, `TRACE`. Keep
`logger.setup()` idempotent (see notes in `R/utils-logging.R`).

### Generated and Non-Package Files

- Do not hand-edit `NAMESPACE`, `man/*.Rd` (roxygen2) or `docs/` (pkgdown).
  Edit the roxygen comments in `R/` and regenerate.
- `local_test/` holds ad-hoc scripts, not package tests; it is excluded by
  `.Rbuildignore`, as are `.claude/`, `docs/` and `_pkgdown.yml`.

### Dependencies

Keep `Imports` minimal. New dependencies need strong justification.


---

## Build, Test, and Document

Run from the package root in R (or via `Rscript -e '...'`):

| Task | Command |
|------|---------|
| Regenerate `man/` and `NAMESPACE` | `devtools::document()` |
| Run tests | `devtools::test()` (files in `tests/testthat/`) |
| Quick check | `MazamaCoreUtils::check_fast()` |
| Full check before release | `MazamaCoreUtils::check_slower()` |
| Build website | `pkgdown::build_site()` (output in `docs/`, config in `_pkgdown.yml`) |
| Install locally | `devtools::install()` |

- New behavior and bug fixes should come with a test in `tests/testthat/`
  (`test-<topic>.R`).
- New exported functions must also be listed in the appropriate `reference:`
  section of `_pkgdown.yml`.
- Update `NEWS.md` (newest entry first, `# MazamaCoreUtils x.y.z`) for any
  user-visible change. The version is defined only in `DESCRIPTION`.
- Vignettes (`vignettes/*.Rmd`: cache management, date parsing, error handling,
  logging) must stay consistent with function documentation.
- Releases: use `/pre-publish`, then push and submit to CRAN manually.


---

## Review Expectations

When reviewing code:

1. Identify correctness issues.
2. Identify operational risks and backward-compatibility concerns.
3. Identify documentation gaps.
4. Suggest low-risk improvements.

Prioritize and communicate recommendations as described in
`.claude/CLAUDE_STYLE_GUIDE.md` (Review Priorities and Communication Style).
Do not assume a rewrite is desired.
