#' S4 class for tree sizes: diameter, height, and crown specifics.
#'
#' @template treesize
#' @template mvdf
#'
#' @family classes and related functions
#'
#' @importClassesFrom mvdf mvdf_obj
#'
#' @exportClass ftt_treesize
methods::setClass("ftt_treesize",
  contains = "mvdf_obj",
  slots = c(
    dbh = "numeric",
    height = "numeric",
    crown = "logical",
    crown_radius = "numeric"
  )
)

#' @export
ftt_treesize <- function(data, metadata = data.frame(), appendix = list()) {

  methods::new("ftt_treesize",
      idx = as.character(data$idx),
      x = as.double(data$x),
      y = as.double(data$y),
      z = as.double(data$z),
      dbh = as.double(data$dbh),
      height = as.double(data$height),
      crown = as.logical(data$crown),
      crown_radius = as.double(data$crown_radius),
      metadata = as.data.frame(metadata),
      appendix = as.list(appendix)
      )

}

#' S4 class for trees to be modeled
#'
#' @template mvdf
#' @template treesize
#'
#' @family classes and related functions
#'
#' @exportClass ftt_tree
methods::setClass("ftt_tree",
  contains = c("ftt_treesize")
)

#' Test to ensure an object meets minimal content requirements
#'
#' @param object The object to test.
#' @param class The S4 class (as a character string) with the slots that
#' `object` needs to have.
#' @param error Logical: Should the function error if the object doesn't contain
#' the other class, or return `FALSE`? Default to `FALSE` (do not error).
#'
#' @export
ftt_contains <- function(object, class, error = FALSE) {

  res <- isS4(object) && methods::is(object, class2 = class)

  if (error && !res) {
    stop(deparse(substitute(object)),
         " must have all slots from ",
         class)
  }

  return(res)

}
