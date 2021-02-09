#' Add code to add primitives to a Blender script
#'
#' This function generates code that will, when run inside Blender, create
#' saplings (using the "add curve sapling" addon) to a scene.
#'
#' @template amp_object
#' @template script
#' @param id_code x
#' @param id_key x
#' @param location x
#' @param ... Additional arguments to pass to the sapling creation call.
#' Currently ignored.
#'
#' @return A length 1 character vector containing the Blender Python script with
#' code for creating saplings added.
#'
#' @name ftt_add_sapling
#'
#' @export
ftt_add_sapling <- function(script,
                            object,
                            id_code = NULL,
                            id_key = NULL,
                            location = NULL,
                            ...) {

  ftt_add_sapling_method(object, script, id_code, id_key, location, ...)

}

setGeneric(
  "ftt_add_sapling_method",
  function(object,
           script,
           id_code = NULL,
           id_key = NULL,
           location = NULL,
           ...) {
    standardGeneric("ftt_add_sapling_method")
  }
)

#' @importClassesFrom mvdf mvdf_obj
setMethod(
  "ftt_add_sapling_method",
  "mvdf_obj",
  function(object,
           script,
           id_code = NULL,
           id_key = NULL,
           location = NULL,
           ...) {

    stopifnot(is.character(script) && (length(script) == 1))
    # At the moment, dots are ignored
    # TODO: Should they be? Probably should replace defaults with ...
    dots <- list(...)

    if (length(dots) == 0) {
      dots <- NULL
    } else {
      dots <- pythonize_booleans(dots)
      dots <- create_options(dots)
    }

    mvdf_df <- mvdf::mvdf(object)
    if (is.null(id_code)) {
      # There is no genus "1", so this returns the default method
      # TODO: Make calling the default method more explicit
      mvdf_df$render_call <- ftt_genus_sapling_lookup(1)
    } else if (id_code == 'species') {
      mvdf_df$render_call <- ftt_species_sapling_lookup(id_key)
    } else {
      mvdf_df$render_call <- ftt_generic_sapling_lookup(id_key, id_code)
    }

    if (is.null(location)) {
      mvdf_df$render_call <- paste0(
        mvdf_df$render_call,
        "\n",
        move_location(mvdf_df)
      )
    }

    render_calls <- paste0(mvdf_df$render_call, collapse = "\n")

    paste0(
      "# %mvdf:addon add_curve_sapling\n",
      script,
      render_calls,
      collapse = "\n"
    )

  }
)

setMethod(
  "ftt_add_sapling_method",
  "ftt_treespp",
  function(object,
           script,
           id_code = NULL,
           id_key = NULL,
           location = NULL,
           ...) {
    ftt_add_sapling(script,
                    mvdf::set_values(object, newclass = "mvdf_obj"),
                    id_code = object@id_code,
                    id_key = object@id_key)
  }
)
