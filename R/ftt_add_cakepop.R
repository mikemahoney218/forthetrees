#' Add code to add primitives to a Blender script
#'
#' This function generates code that will, when run inside Blender, create mesh
#' primitives within a scene.
#'
#' @template amp_object
#' @template script
#' @param height The height of the central bole, in meters.
#' @param dbh The diameter of the central bole, in meters. Cake pops do not
#' taper.
#' @param crown Boolean: draw a "crown" sphere?
#' @param crown_radius The radius of the crown, in meters. The crown is centered
#' on the top of the bole.
#' @param ... Additional arguments to pass to the primitive creation call. The
#' available arguments are different for each primitive, and are documented in
#' the official Blender documentation at
# nolint start
#' \url{https://docs.blender.org/api/blender_python_api_current/bpy.ops.mesh.html}.
# nolint end
#'
#' @return A length 1 character vector containing the Blender Python script with
#' code for creating primitives added.
#'
#' @name ftt_add_cakepop
#'
#' @export
ftt_add_cakepop <- function(script,
                            object,
                            height = NULL,
                            dbh = NULL,
                            crown = TRUE,
                            crown_radius = NULL,
                            ...) {
  ftt_add_cakepop_method(object, script, height, dbh, crown, crown_radius, ...)
}

setGeneric(
  "ftt_add_cakepop_method",
  function(object,
           script,
           height = NULL,
           dbh = NULL,
           crown = TRUE,
           crown_radius = NULL,
           ...) {
    standardGeneric("ftt_add_cakepop_method")
  }
)

#' @rdname ftt_add_cakepop
#' @importClassesFrom mvdf mvdf_obj
setMethod(
  "ftt_add_cakepop_method",
  "mvdf_obj",
  function(object,
           script,
           height = NULL,
           dbh = NULL,
           crown = TRUE,
           crown_radius = NULL,
           ...) {

    if (is.null(height)) height <- 20
    if (is.null(dbh)) dbh <- 15 / 39.37 # inches to meters
    if (is.null(crown_radius)) crown_radius <- dbh * sqrt(dbh * 39.37)

    crown_mvdf <- trunk_mvdf <- object

    mvdf::mvdf(trunk_mvdf)$z <- mvdf::mvdf(trunk_mvdf)$z + (height / 2)

    if (any(crown == FALSE)) {
      mvdf::mvdf(crown_mvdf) <- mvdf::mvdf(crown_mvdf)[crown, ]
    }

    script <- mvdf::add_mesh_primitive(
      script,
      trunk_mvdf,
      "cylinder",
      radius = dbh / 2,
      depth = height,
      ...
    )

    if (!all(crown == FALSE)) {
      mvdf::mvdf(crown_mvdf)$z <- mvdf::mvdf(crown_mvdf)$z + height

      script <- mvdf::add_mesh_primitive(
        script,
        crown_mvdf,
        radius = crown_radius,
        ...
      )
    }

    script

  }
)

#' @rdname ftt_add_cakepop
#' @importClassesFrom mvdf mvdf_simple_material
#' @export
setMethod(
  "ftt_add_cakepop_method",
  "ftt_treesize",
  function(object,
           script,
           height = NULL,
           dbh = NULL,
           crown = TRUE,
           crown_radius = NULL,
           ...) {
    cur_mvdf <- mvdf::mvdf(object)
    ftt_add_cakepop_method(mvdf::set_values(object,
                                            newclass = "mvdf_simple_material"),
                           script,
                           height = cur_mvdf$height,
                           dbh = cur_mvdf$dbh,
                           crown = cur_mvdf$crown,
                           crown_radius = cur_mvdf$crown_radius,
                           ...)
  }
)
