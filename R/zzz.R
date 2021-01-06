#' Standard way to evaluate arguments for class-creating functions
#'
#' This approach enables users to specify arguments as quoted strings
#' referencing column names (passes the first try, passes the second try), as
#' unquoted column names evaluated via rlang's NSE (fails the first try, passes
#' the second try), or as pure values (passes the first try, fails the second
#' try).
#'
#' If the user provides an unquoted string that does not correspond to a column,
#' this function returns NULL.
#'
#' @param data The data frame to try retrieving values from.
#' @param arg The argument to evaluate.
#'
#' @keywords internal
eval_arg <- function(data, arg) {
  arg <- tryCatch(arg, error = function(e) rlang::ensym(arg))
  tryCatch(data[[arg]], error = function(e) arg)
}

#' Standard way to replace & replicate values for class-creation functions
#'
#' @param vec The vector to replace and replicate values throughout
#' @param length_out The target length of the vector
#' @param replace_val The value to replace missing values with
#'
#' @return A vector of length `length_out` with missing values replaced by
#' `replace_val`
#'
#' @keywords internal
calc_val <- function(vec, length_out, replace_val = 0) {
  if (length(vec) == 1 &&
      vec == deparse(substitute(vec))) {
    vec <- NA
  }

  if (all_missing(vec)) vec <- replace_val
  if (any_missing(vec)) vec[which(is_missing(vec))] <- replace_val

  if (length(vec) == 1) vec <- rep(vec, length_out)
  vec
}

#' Check to see if any values in a vector are missing (NULL, NA, INF, or NaN)
#'
#' @param obj The vector of values to check
#'
#' @keywords internal
any_missing <- function(obj) {
  any(is_missing(obj))
}

#' Check to see if all values in a vector are missing (NULL, NA, INF, or NaN)
#'
#' @param obj The vector of values to check
#'
#' @keywords internal
all_missing <- function(obj) {
  all(is_missing(obj))
}

#' Check values in a vector for missingness (NULL, NA, INF, or NaN)
#'
#' @param obj The vector of values to check
#'
#' @keywords internal
is_missing <- function(obj) {
  is.null(obj) |
    is.na(obj) |
    is.infinite(obj) |
    is.nan(obj) |
    length(obj) == 0
}
