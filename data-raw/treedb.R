def_path <- "inst/extdata/tree_defs/"
tree_defs <- vapply(
  list.files(def_path),
  function(x) {
    readChar(paste0(def_path, x), file.info(paste0(def_path, x))$size)
  },
  character(1)
)
tree_defs <- data.frame(
  id = gsub("\\.txt$", "", names(tree_defs)),
  call = tree_defs,
  row.names = NULL
)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "inst/extdata/treedb.db")
DBI::dbWriteTable(con, "tree_defs", tree_defs, overwrite = TRUE)
DBI::dbDisconnect(con, shutdown = TRUE)
