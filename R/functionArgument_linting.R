#' Lint function calls for required named arguments
#'
#' Parse R source code and identify calls to selected functions that are missing
#' required named arguments.
#'
#' Rules are supplied as a named list where each name is a function to check and
#' each value is a character vector of required argument names. A function call
#' passes when all required arguments are supplied by name.
#'
#' This linter only checks whether arguments are named in the call. It does not
#' evaluate code, inspect argument values, or detect unnamed positional
#' arguments.
#'
#' @param filePath Path to a single R source file.
#' @param dirPath Path to a directory containing R source files.
#' @param rules Named list of linting rules. Each list name is a function name
#'   and each value is a character vector of required named arguments.
#' @param fullPath Logical specifying whether returned file paths should be
#'   absolute paths. If `FALSE`, only base file names are returned.
#'
#' @return
#' A tibble describing matching function calls, with columns:
#'
#' \describe{
#'   \item{file}{Source file path or file name.}
#'   \item{line_number}{Line number where the function call begins.}
#'   \item{column_number}{Column number where the function call begins.}
#'   \item{function_name}{Name of the function being checked.}
#'   \item{named_args}{List column containing named arguments used in the call.}
#'   \item{includes_required}{Logical indicating whether all required named
#'     arguments were supplied.}
#' }
#'
#' @section Limitations:
#' This linter only detects named arguments. For example, `foo(x = bar, "baz")`
#' is treated as specifying the named argument `x`, but the value `bar` and the
#' unnamed argument `"baz"` are not inspected.
#'
#' @name lintFunctionArgs
#' @aliases lintFunctionArgs_file lintFunctionArgs_dir
#'
#' @examples
#' \dontrun{
#' rules <- list(
#'   fn_one = "x",
#'   fn_two = c("foo", "bar")
#' )
#'
#' lintFunctionArgs_file(
#'   filePath = "local_test/timezone_lint_test_script.R",
#'   rules = rules
#' )
#'
#' lintFunctionArgs_dir(
#'   dirPath = "./R",
#'   rules = MazamaCoreUtils::timezoneLintRules
#' )
#' }
NULL
NULL

#' @rdname lintFunctionArgs
#' @export
lintFunctionArgs_file <- function(
  filePath = NULL,
  rules = NULL,
  fullPath = FALSE
) {

  # Validate input ------------------------------------------------------------

  stopIfNull(filePath)
  stopIfNull(rules)

  if ( !is.list(rules) || is.null(names(rules)) ) {
    stop("rules must be a named list")
  }

  if ( !is.character(filePath) || length(filePath) != 1 ) {
    stop("filePath must be a length 1 character vector")
  }

  normFilePath <- normalizePath(filePath)

  if ( !utils::file_test("-f", normFilePath) ) {
    stop("filePath must point to a file, not a directory")
  }


  # Parse file ----------------------------------------------------------------

  parsedData <-
    normFilePath %>%
    parse(keep.source = TRUE) %>%
    utils::getParseData() %>%
    tibble::as_tibble()


  # Collect functions and arguments -----------------------------------------

  # Given IDs as names, this vector outputs the IDs' parent IDs
  lookupParent <-
    parsedData %>%
    dplyr::select(.data$id, .data$parent) %>%
    tibble::deframe()

  # Group function arguments by which function they belong to
  functionArgs <-
    parsedData %>%
    dplyr::filter(.data$token == "SYMBOL_SUB") %>%
    dplyr::group_by(.data$parent) %>%
    dplyr::summarise(named_args = list(.data$text)) %>%
    dplyr::rename(id = .data$parent)

  # Pair function calls with their arguments
  functionCalls <-
    parsedData %>%
    dplyr::mutate(lookup_pid = lookupParent[as.character(.data$parent)]) %>%
    dplyr::filter(.data$token == "SYMBOL_FUNCTION_CALL") %>%
    dplyr::select(
      line_number = .data$line1,
      column_number = .data$col1,
      function_name = .data$text,
      id = .data$lookup_pid
    ) %>%
    dplyr::left_join(functionArgs, by = "id") %>%
    dplyr::select(-.data$id)


  # Check function arguments ------------------------------------------------

  fileString <- if ( fullPath ) normFilePath else basename(normFilePath)

  results <-
    functionCalls %>%
    dplyr::filter(.data$function_name %in% names(rules)) %>%
    dplyr::mutate(
      includes_required = purrr::map2_lgl(
        .data$named_args,
        .data$function_name,
        ~all(rules[[.y]] %in% .x)
      ),
      file = fileString
    ) %>%
    dplyr::select(.data$file, dplyr::everything())

  return(results)

}


#' @rdname lintFunctionArgs
#' @export
lintFunctionArgs_dir <- function(
  dirPath = "./R",
  rules = NULL,
  fullPath = FALSE
) {

  # Validate input -------------------------------------------------------------

  stopIfNull(rules)

  if ( !is.list(rules) || is.null(names(rules)) )
    stop("rules must be a named list")

  if ( !is.character(dirPath) || length(dirPath) != 1 )
    stop("dirPath must be a length 1 character vector")

  normDirPath <- normalizePath(dirPath)

  if ( !utils::file_test("-d", normDirPath) )
    stop("filePath must point to a directory, not a file")


  # Lint files -----------------------------------------------------------------

  results <- normDirPath %>%
    list.files(pattern = "\\.R$", full.names = TRUE, recursive = TRUE) %>%
    purrr::map_dfr(lintFunctionArgs_file, rules, fullPath)

  return(results)

}
