#' @slot dbh Numeric: diameter at breast height, in meters. May include missing
#' values, which will be handled differently by different rendering functions.
#' @slot height Numeric: bole height, in meters. May include missing values,
#' which will be handled differently by different rendering functions. Height is
#' understood as the total height of the object to be rendered (top of the stem
#' and crown, if applicable); different importer functions may determine this
#' differently.
#' @slot crown Logical: does the tree have leaves? May include missing values,
#' which will be handled differently by different rendering functions.
#' @slot crown_radius Numeric: radius of the crown, in meters, at its widest
#' point. May include missing values, which will be handled differently by
#' different rendering functions.
