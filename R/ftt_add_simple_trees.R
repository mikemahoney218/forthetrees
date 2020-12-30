#' Add code to add simple trees to a Blender script
#'
#' This function generates code that will, when run inside Blender, create
#' simple trees using the sapling tree gen add-on.
#'
#' @template amp_object
#' @template script
#' @param ... Ignored
#'
#' @return A length 1 character vector containing the Blender Python script with
#' code for creating trees added.
#'
#' @name ftt_add_simple_trees
#'
setGeneric(
  "ftt_add_simple_trees",
  function(object,
           script,
           ...) {
    standardGeneric("ftt_add_simple_trees")
  }
)

#' @rdname ftt_add_simple_trees
setMethod(
  "ftt_add_simple_trees",
  "mvdf_obj",
  function(object,
           script,
           ...) {
    stopifnot(is.character(script) && (length(script) == 1))

    paste0(
      script,
      "\n",
      glue::glue_collapse(
        glue::glue(
          "bpy.ops.{category}.primitive_{primitive}_add({arguments})",
          category = category,
          primitive = primitive,
          arguments = arguments
        ),
        sep = "\n"
      )
    )
  }
)
