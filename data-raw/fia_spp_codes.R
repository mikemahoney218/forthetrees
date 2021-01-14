library(dplyr)
library(tidyr)
library(stringr)
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
DBI::dbWriteTable(con, "common_name_lookup", plants_code, overwrite = TRUE)
DBI::dbDisconnect(con, shutdown = TRUE)
