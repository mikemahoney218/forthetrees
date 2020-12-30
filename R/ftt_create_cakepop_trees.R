#' @export
ftt_create_cakepop_trees <- function(frontmatter, object, crowntype = "uv") {
  if (!methods::is(object, "ftt_treesize")) {
    stop(
      "ftt_create_cakepop_trees requires an object with all the same fields",
      " as fft_treesize."
    )
  }

  tree_mvdf <- mvdf(object = object)
  # Reject any no-DBH points
  tree_mvdf <- tree_mvdf[-which(is.na(tree_mvdf$dbh)), ]

  # Set NA heights to sensible, ish, values
  # For the moment, this is using Monserud's formula with mean of coefficients
  # from non-mullberry species in this paper:
  # https://www.ncrs.fs.fed.us/pubs/jrnl/jrnl_colbert001.pdf
  tree_mvdf[which(is.na(tree_mvdf$height)), ]$height <-
    1.37 + exp(5.4 +
      (-4.15) *
        tree_mvdf[which(is.na(tree_mvdf$dbh)), ]$dbh *
        (-0.571))

  # If you don't specify no crown, you get a crown
  tree_mvdf[which(
    is.na(tree_mvdf$crown)
  ), ]$crown <- rep(
    TRUE,
    sum(is.na(tree_mvdf$crown))
  )

  # Fill in missing crown radius values with sensible, ish, values
  # For the moment, using mean coefficients from:
  # https://www.srs.fs.usda.gov/pubs/ja/ja_lockhart009.pdf
  # Note for the future, another similar reference:
  # https://www.fs.fed.us/ne/newtown_square/publications/research_papers/pdfs/scanned/OCR/ne_rp610.pdf # nolint
  tree_mvdf[which(
    is.na(tree_mvdf$crown_radius)),
    ]$crown_radius <- 0.833 + (0.07 * tree_mvdf[which(
      is.na(tree_mvdf$crown_radius)),
    ]$dbh)

  tree_mvdf$stem_str <- paste0(
    "bpy.ops.mesh.primitive_cylinder_add(",
    "radius=", tree_mvdf$dbh / 2,
    ", depth=", tree_mvdf$height / 2,
    ", location=(", tree_mvdf$x,
    ", ", tree_mvdf$y,
    ", ", tree_mvdf$z + (tree_mvdf$height / 2),
    "))"
  )

  tree_mvdf$crown_str <- ifelse(
    tree_mvdf$crown,
    paste0(
      "bpy.ops.mesh.primitive_",
      crowntype,
      "_sphere_add(radius=",
      tree_mvdf$crown_radius,
      ", location=(",
      tree_mvdf$x,
      ",",
      tree_mvdf$y,
      ",",
      tree_mvdf$z + (tree_mvdf$height * 0.75),
      "))"
    ),
    "")

  paste0(
    frontmatter,
    "\n",
    paste0(tree_mvdf[["stem_str"]], collapse = "\n"),
    "\n",
    paste0(tree_mvdf[["crown_str"]], collapse = "\n"),
    collapse = "\n"
  )

}
