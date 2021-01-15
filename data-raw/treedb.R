library(dplyr)
library(tidyr)
library(stringr)

def_path <- "inst/extdata/tree_defs/"
tree_defs <- vapply(
  list.files(def_path),
  function(x) {
    readChar(paste0(def_path, x), file.info(paste0(def_path, x))$size)
  },
  character(1)
)
tree_defs <- data.frame(
  render_key = gsub("\\.txt$", "", names(tree_defs)),
  render_call = tree_defs,
  row.names = NULL
)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "inst/extdata/treedb.db")
DBI::dbWriteTable(con, "sapling_tree_defs", tree_defs, overwrite = TRUE)
DBI::dbDisconnect(con, shutdown = TRUE)

species_code_url <- "https://www.fia.fs.fed.us/library/field-guides-methods-proc/docs/2019/2019%20Master%20Species%20FGver90_10_01_2019_rev_2_10_2020.xlsx" # nolint
download.file(species_code_url, "data-raw/fia_species_code.xlsx")
species_code <- readxl::read_excel("data-raw/fia_species_code.xlsx",
                                   na = c("", "."),
                                   guess_max = 1500)
species_code <- species_code[5:11]
names(species_code) <- gsub(" ", "_", tolower(names(species_code)))
species_code <- species_code %>%
  mutate(common_name = str_split(common_name, " or ")) %>%
  unnest(common_name) %>%
  mutate(common_name = str_split(common_name, ", ")) %>%
  unnest(common_name) %>%
  mutate(across(where(is.character), tolower))

fia_code <- species_code %>%
  distinct(fia_code, genus, species)

stopifnot(nrow(fia_code) == length(unique(fia_code$fia_code)))

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "inst/extdata/treedb.db")
DBI::dbWriteTable(con, "fia_code_lookup", fia_code, overwrite = TRUE)
DBI::dbDisconnect(con, shutdown = TRUE)

plants_code <- species_code %>%
  distinct(plants_code, genus, species)

stopifnot(nrow(plants_code) == length(unique(plants_code$plants_code)))

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "inst/extdata/treedb.db")
DBI::dbWriteTable(con, "plants_code_lookup", plants_code, overwrite = TRUE)
DBI::dbDisconnect(con, shutdown = TRUE)

common_name <- species_code %>%
  drop_na(common_name) %>%
  distinct(common_name, genus, species) %>%
  group_by(common_name, genus) %>%
  mutate(n = n(),
         species = ifelse(n > 1, NA, species)) %>%
  group_by(common_name) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  select(-n)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "inst/extdata/treedb.db")
DBI::dbWriteTable(con, "common_name_lookup", common_name, overwrite = TRUE)
DBI::dbDisconnect(con, shutdown = TRUE)

species <- read.csv("data-raw/species.csv")
genus <- read.csv("data-raw/genus.csv")
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "inst/extdata/treedb.db")
DBI::dbWriteTable(con, "genus_lookup", genus, overwrite = TRUE)
DBI::dbWriteTable(con, "species_lookup", species, overwrite = TRUE)
DBI::dbDisconnect(con, shutdown = TRUE)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "inst/extdata/treedb.db")
DBI::dbExecute(con, "EXPORT DATABASE 'inst/extdata/treedb'")
DBI::dbDisconnect(con, shutdown = TRUE)
