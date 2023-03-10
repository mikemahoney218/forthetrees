#' S4 class for tree sizes: diameter, height, and crown specifics.
#'
#' @template treesize
#' @template mvdf
#'
#' @family classes and related functions
#'
#' @importClassesFrom mvdf mvdf_simple_material
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
#' @param translate_colors Passed to [mvdf::mvdf_simple_material] (but included
#' as a separate argument to help with auto-complete).
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
                                    translate_colors = translate_colors,
                                    ...)
  res_mvdf <- mvdf::mvdf(res)

  if (!is.null(data)) {
    dbh <- eval_arg(data, dbh)
    height <- eval_arg(data, height)
    crown <- eval_arg(data, crown)
    crown_radius <- eval_arg(data, crown_radius)
  }

  length_out <- length(res_mvdf$idx)

  tree_params <- solve_tree(dbh = dbh,
                            height = height,
                            crown_radius = crown_radius)

  dbh <- tree_params[["dbh"]]
  height <- tree_params[["height"]]
  crown_radius <- tree_params[["crown_radius"]]

  dbh <- calc_val(dbh, length_out, dbh)
  height <- calc_val(height, length_out, height)
  crown <- calc_val(crown, length_out, TRUE)
  crown_radius <- calc_val(crown_radius, length_out, crown_radius)

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

#' S4 class for tree species: ID codes and keys to look up rendering methods.
#'
#' @template treespp
#' @template mvdf
#'
#' @family classes and related functions
#'
#' @importClassesFrom mvdf mvdf_obj
#'
#' @exportClass ftt_treespp
methods::setClass("ftt_treespp",
                  contains = "mvdf_obj",
                  slots = c(
                    id_code = "character",
                    id_key = "character"
                  )
)

setValidity("ftt_treespp", function(object) {
  error <- vector("character")
  n_issue <- 1

  id_code_opts <- c("fia_code",
                    "plants_code",
                    "common_name",
                    "species",
                    "genus")

  if (!(all(object@id_code %in% id_code_opts))) {
    error[n_issue] <- paste("@id_code must be one of: ",
                            paste0(id_code_opts,
                                   collapse = ", "))
    n_issue <- n_issue + 1
  }

  if (n_issue > 1) {
    return(paste0(error, collapse = "\n"))
  }

  return(TRUE)

})


#' Create an `ftt_treespp` object
#'
#' @param data Optionally, a data frame containing all the data necessary to
#' create an `ftt_treespp` object. If `NULL`, all other arguments are
#' interpreted as data to use in constructing the object; if not `NULL`,
#' arguments are interpreted as the names of columns in `data` containing the
#' values for each slot.
#' @param id_code The code indicating what sort of identifier is stored in
#' id_key -- one of fia_code, plants_code, common_name, genus, or species.
#' @param id_key The key identifying the tree species in the schema referenced
#' by id_code.
#' @param ... Additional arguments passed to [mvdf::mvdf_obj].
#'
#' @return An object of class `ftt_treespp`.
#'
#' @export
ftt_treespp <- function(data = NULL,
                        id_code = "id_code",
                        id_key = "id_key",
                        ...) {

  res <- mvdf::mvdf_obj(data = data, ...)
  res_mvdf <- mvdf::mvdf(res)

  if (!is.null(data)) {
    id_code <- eval_arg(data, id_code)
    id_key <- eval_arg(data, id_key)
  }

  length_out <- length(res_mvdf$idx)

  id_code <- calc_val(id_code, length_out, "species")
  id_key <- calc_val(id_key, length_out, " ")

  methods::new("ftt_treespp",
               x = as.double(res_mvdf$x),
               y = as.double(res_mvdf$y),
               z = as.double(res_mvdf$z),
               idx = as.character(res_mvdf$idx),
               metadata = as.data.frame(mvdf::metadata(res)),
               appendix = as.list(mvdf::appendix(res)),
               id_code = as.character(id_code),
               id_key = as.character(id_key)
               )

}

