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
  contains = "mvdf_simple_material",
  slots = c(
    dbh = "numeric",
    height = "numeric",
    crown = "logical",
    crown_radius = "numeric"
  )
)

#' Create an `ftt_treesize` object
#'
#' @param data Optionally, a data frame containing all the data necessary to
#' create an `ftt_treesize` object. If `NULL`, all other arguments are
#' interpreted as data to use in constructing the object; if not `NULL`,
#' arguments are interpreted as the names of columns in `data` containing the
#' values for each slot.
#' @param dbh Numeric: diameter at breast height, in meters. May include missing
#' values, which will be handled differently by different rendering functions.
#' @param height Numeric: bole height, in meters. May include missing values,
#' which will be handled differently by different rendering functions. Height is
#' understood as the total height of the object to be rendered (top of the stem
#' and crown, if applicable); different importer functions may determine this
#' differently.
#' @param crown Logical: does the tree have leaves? May include missing values,
#' which will be handled differently by different rendering functions.
#' @param crown_radius Numeric: radius of the crown, in meters, at its widest
#' point. May include missing values, which will be handled differently by
#' different rendering functions.
#' @param ... Additional arguments passed to [mvdf::mvdf_simple_material].
#'
#' @return An object of class `ftt_treesize`.
#'
#' @export
ftt_treesize <- function(data = NULL,
                         dbh = "dbh",
                         height = "height",
                         crown = "crown",
                         crown_radius = "crown_radius",
                         ...,
                         translate_colors = FALSE) {

  res <- mvdf::mvdf_simple_material(data = data,
                                    ...)
  res_mvdf <- mvdf::mvdf(res)

  if (!is.null(data)) {
    dbh <- eval_arg(data, dbh)
    height <- eval_arg(data, height)
    crown <- eval_arg(data, crown)
    crown_radius <- eval_arg(data, crown_radius)
  }

  length_out <- length(res_mvdf$idx)

  dbh <- calc_val(dbh, length_out, 0.381)
  height <- calc_val(height, length_out, 20)
  crown <- calc_val(crown, length_out, 9.25)
  crown_radius <- calc_val(crown_radius, length_out, TRUE)

  methods::new("ftt_treesize",
               x = as.double(res_mvdf$x),
               y = as.double(res_mvdf$y),
               z = as.double(res_mvdf$z),
               idx = as.character(res_mvdf$idx),
               metadata = as.data.frame(mvdf::metadata(res)),
               appendix = as.list(mvdf::appendix(res)),
               diffuse_color = as.character(res_mvdf$diffuse_color),
               metallic = as.numeric(res_mvdf$metallic),
               roughness = as.numeric(res_mvdf$roughness),
               dbh = as.double(dbh),
               height = as.double(height),
               crown = as.logical(crown),
               crown_radius = as.double(crown_radius)
               )

}
