#' @export
ftt_execute_render <- function(script) {

  blender <- Sys.which("blender")

  if (file.exists(script)) {
    system2(blender, args = paste("-b -P", script))
  } else {
    scriptfile <- tempfile(fileext = ".py")
    writeLines(script, scriptfile)
    system2(blender, args = paste("-b -P", scriptfile))
  }
}
