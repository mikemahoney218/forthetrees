#' @export
ftt_add_sapling <- function(script,
                            object,
                            ...) {

  ftt_add_sapling_method(object, script, ...)

}

setGeneric(
  "ftt_add_sapling_method",
  function(object,
           script,
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
           ...) {



    paste0(
      "# %mvdf:addon add_curve_sapling\n",
      script,
      "bpy.ops.curve.tree_add()",
      collapse = "\n"
    )

  }
)
