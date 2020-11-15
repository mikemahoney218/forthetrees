#' @export
ftt_create_blender_frontmatter <- function(imports = c("bpy",
                                                       "mathutils",
                                                       "math"),
                                           delete = c(
                                             "Cube",
                                             "Camera",
                                             "Light"
                                           )) {
  if (!is.null(names(imports))) {
    # if any aliases are used, use all as aliases
    names(imports)[which(
      names(imports) == imports
    )] <- imports[which(names(imports) == imports)]

    imports <- paste(imports, "as", names(imports))

  }

  out_imports <- vector("list", length = length(imports) + 1)
  for (i in seq_len(length(imports))) {
    out_imports[[i]] <- paste("import", imports[[i]])
    out_imports[[i + 1]] <- ""
  }

  out_delete <- vector("list", length = length(delete) + 1)
  for (i in seq_len(length(delete))) {
    out_delete[[i]] <- paste0('bpy.data.objects.remove(bpy.data.objects["',
                              delete[[i]],
                              '"], do_unlink=True)')
    out_delete[[i + 1]] <- ""
  }

  return(paste0(
    paste0(out_imports, collapse = "\n"),
    "\n",
    paste0(out_delete, collapse = "\n"),
    "",
    collapse = "\n"))

}
