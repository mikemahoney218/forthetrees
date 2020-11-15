#' @export
ftt_create_blender_endmatter <- function(script,
                                         filepath = tempfile(fileext = ".blend")
                                         ) {

  paste0(
    script,
    "\n\n",
    "bpy.ops.wm.save_as_mainfile(filepath='",
    filepath,
    "')\n"
  )

}
