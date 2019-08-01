tz_lint_file <- function(file_path, tz_lint_opts = NULL) {

  if (is.null(tz_lint_opts)) {

    tz_lint_opts <- list(
      function_names = c("parse_date_time", "with_tz", "now", "strftime"),
      parameter_names = c("tz", "tzone", "timezone")
    )

  }


  norm_fp <- normalizePath(file_path)

  parsed_data <-
    norm_fp %>%
    parse(keep.source = TRUE) %>%
    getParseData()

  function_calls_tbl <- parsed_data %>%
    dplyr::filter(
      .data$token == "SYMBOL_FUNCTION_CALL",
      .data$text %in% tz_lint_opts$function_names
    ) %>%
    dplyr::mutate(fn_arguments = get_function_args(parsed_data, .data$parent))


}


get_function_args <- function(parsed_data, parent_id) {



}
