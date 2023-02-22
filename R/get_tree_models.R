#' Download 3D tree models for Unity
#'
#' This is a simple helper function downloading the tree models stored at
#' https://github.com/mikemahoney218/tree_models .
#'
#' @param directory Optionally, the directory to extract the downloaded models
#' in. If NULL, the default, saves to `tools::R_user_dir("forthetrees")`.
#'
#' @export
get_tree_models <- function(directory = NULL) {

  if (is.null(directory)) {
    directory <- tools::R_user_dir("forthetrees")
  }
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
  stopifnot(dir.exists(directory))

  dl <- tempfile(fileext = ".zip")
  download.file(
    "https://github.com/mikemahoney218/tree_models/archive/refs/heads/main.zip",
    dl
  )
  unzip(
    dl,
    exdir = directory
  )
}
