---
description: Comprehensive project-wide code review covering correctness, data conventions, test coverage, API stability, and documentation. Run before any version bump.
allowed-tools: Read, Bash
---

Please conduct a comprehensive code review of this project.

## Scope

Review the entire codebase as a cohesive system. Reference these files for
context on design decisions, review priorities, and communication expectations:

- `.claude/CLAUDE_ARCHITECTURE.md` — module dependencies, design decisions, API contract
- `.claude/CLAUDE_STYLE_GUIDE.md` — review priorities and communication style
- `CLAUDE.md` — project conventions and constraints

- **Source:** `R/` — all files (roxygen2 comments included)
- **Tests:** `tests/testthat/` — all test files
- **Package metadata:** `DESCRIPTION`, `NAMESPACE`, `NEWS.md`, `_pkgdown.yml`
- **Vignettes:** `vignettes/` — runnable examples
- Ignore generated output (`man/`, `docs/`) except to check it is current, and
  treat `local_test/` as scratch, not tests.

## Critical Areas for This Project

### Timezone Correctness (Load-Bearing)
Do all date-time functions require an explicit, validated `timezone` and return
`POSIXct` in that timezone? Is there any use of `Sys.time()`, `Sys.Date()`, or
timezone-less `as.POSIXct()`/`lubridate::now()`? Are end-of-period rules in
`dateRange()`/`timeRange()` and mixed-format parsing in `parseDatetime()`
preserved?

### Logging Behavior
Is the legacy `logger.*()` API, the `"MazamaCoreUtils"` namespace, the fixed
logger indices (console = 1, files = 2-6), and the exported level constants
intact? Does `logger.setup()` remain idempotent? Does `stopOnError()` log only
when logging is initialized?

### Input Validation and Error Handling
Do required arguments use `stopIfNull()` and optional ones `setIfNull()` (with
the result actually assigned)? Do errors name the offending argument? Are there
silent failures, especially in `try()` blocks and the `loadDataFile()` fallback?

### Location Handling
Are longitude/latitude bounds, `NA` handling, `(0, 0)` handling and
`invalidID` behavior consistent across `validateLonLat*()`,
`createLocationMask()` and `createLocationID()`?

### API Stability and Backward Compatibility
Are exported names, argument order, defaults and return types unchanged? Does
`NAMESPACE` match roxygen `@export` tags? Are changes recorded in `NEWS.md`,
and should the `DESCRIPTION` version change?

### Dependencies and CRAN Readiness
Are `Imports` truly needed and used with `pkg::` or `importFrom`? Any new
dependency? Are examples fast, offline-safe and wrapped in `\dontrun{}` where
needed?

### Testing and Edge Cases
Do tests in `tests/testthat/` cover `NULL`, `NA`, zero-length and invalid
inputs and error paths? Note untested exports (e.g. `loadDataFile()`, HTML
helpers, linting functions).

## Provide

For each issue found, provide:

1. **Location** — file and line number (or section name)
2. **Issue** — what is wrong or could be better
3. **Priority** — high / medium / low
   - **High:** correctness bug, missing critical test, backward-compatibility risk, security issue
   - **Medium:** clarity, maintainability, edge case not covered
   - **Low:** style, minor optimization, nice-to-have improvement
4. **Suggested fix** — concise recommendation
5. **Why** — brief rationale (references to design decisions or maintainer philosophy are helpful)

## Do Not

- Propose major architectural rewrites unless a clear problem exists.
- Suggest adding dependencies without strong justification.
- Recommend optimizations unless performance is a demonstrated problem.
- Assume the codebase needs refactoring; small, incremental improvements are preferred.

Do not modify any files.
