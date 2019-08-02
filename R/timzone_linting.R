tz_lint_file <- function(file_path, tz_lint_opts = NULL) {


  # Set defaults ------------------------------------------------------------

  if (is.null(tz_lint_opts)) {

    tz_lint_opts <- list(
      "parse_date_time" = "tz",
      "with_tz" = "tzone",
      "now" = "tzone",
      "strftime" = "tz"
    )

  }


  # Regularize input --------------------------------------------------------

  norm_fp <- normalizePath(file_path)


  # Parse file --------------------------------------------------------------

  parsed_data <-
    norm_fp %>%
    parse(keep.source = TRUE) %>%
    getParseData() %>%
    tibble::as_tibble()


  # Collect functions and arguments -----------------------------------------

  # Given IDs as names, this vector outputs the IDs' parent IDs
  lookup_parent <- parsed_data %>%
    dplyr::select(.data$id, .data$parent) %>%
    tibble::deframe()

  # Group function arguments by which function they belong to
  function_args <- parsed_data %>%
    dplyr::filter(.data$token == "SYMBOL_SUB") %>%
    dplyr::group_by(.data$parent) %>%
    dplyr::summarise(named_arguments = list(.data$text)) %>%
    dplyr::rename(id = .data$parent)

  function_calls <- parsed_data %>%
    dplyr::mutate(lookup_pid = lookup_parent[as.character(.data$parent)]) %>%
    dplyr::filter(.data$token == "SYMBOL_FUNCTION_CALL") %>%
    dplyr::select(
      line_number = .data$line1,
      column_number = .data$col1,
      function_name = .data$text,
      id = .data$lookup_pid
    ) %>%
    dplyr::left_join(function_args, by = "id") %>%
    dplyr::select(-.data$id)


  # Check timezone arguments ------------------------------------------------

  tz_arg_check <- function_calls %>%
    dplyr::filter(.data$function_name %in% names(tz_lint_opts)) %>%
    dplyr::mutate(
      has_arg = purrr::map2_lgl(
        .data$named_arguments, .data$function_name,
        ~purrr::has_element(.x, tz_lint_opts[[.y]])
      )
    )

  return(tz_arg_check)


}


tz_lint_package <- function(file_dir = "./R", tz_lint_opts = NULL) {


  # Set defaults ------------------------------------------------------------

  if (is.null(tz_lint_opts)) {

    tz_lint_opts <- list(
      "parse_date_time" = "tz",
      "with_tz" = "tzone",
      "now" = "tzone",
      "strftime" = "tz"
    )

  }


  # Regularize input --------------------------------------------------------

  file_paths <- file_dir %>%
    normalizePath() %>%
    fs::dir_ls(recurse = TRUE, type = "file", glob = "*.R")


  # Lint files --------------------------------------------------------------

  results <- file_paths %>%
    purrr::map_dfr(tz_lint_file, tz_lint_opts, .id = "file_path")

  return(results)

}
