
lintFunctionArgs_file <- function(path, rules) {

  # Regularize input --------------------------------------------------------

  normFilePath <- normalizePath(path)


  # Parse file --------------------------------------------------------------

  parsedData <-
    normFilePath %>%
    parse(keep.source = TRUE) %>%
    utils::getParseData() %>%
    tibble::as_tibble()


  # Collect functions and arguments -----------------------------------------

  # Given IDs as names, this vector outputs the IDs' parent IDs
  lookupParent <- parsedData %>%
    dplyr::select(.data$id, .data$parent) %>%
    tibble::deframe()

  # Group function arguments by which function they belong to
  functionArgs <- parsedData %>%
    dplyr::filter(.data$token == "SYMBOL_SUB") %>%
    dplyr::group_by(.data$parent) %>%
    dplyr::summarise(named_arguments = list(.data$text)) %>%
    dplyr::rename(id = .data$parent)

  # Pair function calls with their arguments
  functionCalls <- parsedData %>%
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

  argCheck <- functionCalls %>%
    dplyr::filter(.data$function_name %in% names(rules)) %>%
    dplyr::mutate(
      has_arg = purrr::map2_lgl(
        .data$named_arguments, .data$function_name,
        ~purrr::has_element(.x, rules[[.y]])
      )
    )

  return(argCheck)

}



lintFunctionArgs_directory <- function(path = "./R", rules) {

  # Regularize input --------------------------------------------------------

  filePaths <- path %>%
    normalizePath() %>%
    list.files(pattern = "\\.R$", full.names = TRUE, recursive = TRUE) %>%
    purrr::set_names()


  # Lint files --------------------------------------------------------------

  results <- filePaths %>%
    purrr::map_dfr(lintFunctionArgs_file, rules, .id = "file_path")

  return(results)

}
