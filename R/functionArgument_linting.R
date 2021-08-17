#' @title Lint a source file's function arguments
#'
#' @description
#' This function parses an R Script file, grouping function calls and the named
#' arguments passed to those functions. Then, based on a set of rules, it is
#' determined if functions of interest have specific named arguments specified.
#'
#' @param filePath Path to a file, given as a length one character vector.
#' @param dirPath Path to a directory, given as a length one character vector.
#' @param rules A named list where the name of each element is a function name,
#'   and the value is a character vector of the named argument to check for. All
#'   arguments must be specified for a function to "pass".
#' @param fullPath Logical specifying whether to display absolute paths.
#'
#' @return A \code{\link[tibble]{tibble}} detailing the results of the lint.
#'
#' @section Linting Output:
#' The output of the function argument linter is a tibble with the following
#' columns:
#'
#' \describe{
#'   \item{file_path}{path to the source file}
#'   \item{line_number}{Line of the source file the function is on}
#'   \item{column_number}{Column of the source file the function starts at}
#'   \item{function_name}{The name of the function}
#'   \item{named_args}{A vector of the named arguments passed to the function}
#'   \item{includes_required}{True iff the function specifies all of the named
#'     arguments required by the given rules}
#' }
#'
#' @section Limitations:
#' This function is only able to test for named arguments passed to a function.
#' For example, it would report that \code{foo(x = bar, "baz")} has specified
#' the named argument \code{x}, but not that \code{bar} was the value of the
#' argument, or that \code{"baz"} had been passed as an unnamed argument.
#'
#' @name lintFunctionArgs
#' @aliases lintFunctionArgs_file lintFunctionArgs_dir
#'
#' @examples
#' \dontrun{
#' library(MazamaCoreUtils)
#'
#' # Example rule list for checking
#' exRules <- list(
#'   "fn_one" = "x",
#'   "fn_two" = c("foo", "bar")
#' )
#'
#' # Example of using included timezone argument linter
#' lintFunctionArgs_file(
#'   "local_test/timezone_lint_test_script.R",
#'   MazamaCoreUtils::timezoneLintRules
#' )
#' }
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
    stop("rules must be a named list.")
  }

  if ( !is.character(filePath) || length(filePath) != 1 ) {
    stop("filePath must be a length 1 character vector.")
  }

  normFilePath <- normalizePath(filePath)

  if ( !utils::file_test("-f", normFilePath) ) {
    stop("filePath must point to a file, not a directory.")
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

  if ( !fullPath ) fileString <- basename(normFilePath)

  results <-
    functionCalls %>%
    dplyr::filter(.data$function_name %in% names(rules)) %>%
    dplyr::mutate(
      includes_required = purrr::map2_lgl(
        .data$named_args, .data$function_name,
        ~purrr::has_element(.x, rules[[.y]])
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
    stop("rules must be a named list.")

  if ( !is.character(dirPath) || length(dirPath) != 1 )
    stop("dirPath must be a length 1 character vector.")

  normDirPath <- normalizePath(dirPath)

  if ( !utils::file_test("-d", normDirPath) )
    stop("filePath must point to a directory, not a file.")


  # Lint files -----------------------------------------------------------------

  results <- normDirPath %>%
    list.files(pattern = "\\.R$", full.names = TRUE, recursive = TRUE) %>%
    purrr::map_dfr(lintFunctionArgs_file, rules, fullPath)

  return(results)

}
