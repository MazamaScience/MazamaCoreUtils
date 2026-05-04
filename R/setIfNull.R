#' Set a variable to a default value if it is NULL
#'
#' Returns `default` when `target` is `NULL`; otherwise returns `target`
#' unchanged.
#'
#' This is useful for assigning default values to optional arguments while
#' preserving any user-supplied value exactly as provided.
#'
#' Optionally, `enforceType` may be used to coerce the returned value to a
#' specific type. This coercion is applied after the `NULL` check and affects
#' both `target` and `default`.
#'
#' @param target Object to test for `NULL`.
#' @param default Object to return when `target` is `NULL`.
#' @param enforceType Optional character string specifying the suffix of an
#'   `as.*()` coercion function to apply to the returned value. For example,
#'   `"double"` uses `as.double()`, `"character"` uses `as.character()`,
#'   and `"Date"` uses `as.Date()`.
#'
#'   If `NULL` (the default), no coercion is performed.
#'
#' @return
#' The value of `target` if it is not `NULL`; otherwise `default`.
#'
#' If `enforceType` is specified, the returned value is coerced using the
#' corresponding `as.*()` function.
#'
#' @export
#'
#' @examples
#' setIfNull(NULL, "foo")
#' setIfNull(10, 0)
#' setIfNull("15", 0)
#'
#' # User-supplied values are returned unchanged
#' setIfNull("15", 0)
#' setIfNull("mean", 0)
#' setIfNull(mean, 0)
#'
#' # Optional type enforcement
#' setIfNull("15", 0, enforceType = "double")
#' setIfNull(NULL, "15", enforceType = "integer")
#'
setIfNull <- function(
    target,
    default,
    enforceType = NULL
) {

  # Return target or default --------------------------------------------------

  result <- if (is.null(target)) default else target


  # Optionally enforce type ---------------------------------------------------

  if (!is.null(enforceType)) {

    conversionFuncName <- paste0("as.", enforceType)

    if (!exists(conversionFuncName, mode = "function")) {
      stop(
        sprintf(
          "No coercion function '%s()' found.",
          conversionFuncName
        )
      )
    }

    conversionFunc <-
      get(conversionFuncName, mode = "function")

    result <- conversionFunc(result)

  }

  # Return result -------------------------------------------------------------

  return(result)

}
