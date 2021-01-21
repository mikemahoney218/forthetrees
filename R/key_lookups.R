#' Internal function providing key:value matching for render commands
#'
#' @param code The code to look up -- FIA code, PLANTS shortcode, or common name
#' @param alias A string indicating the calling alias function -- "fia_code" for
#' ftt_fia_sapling_lookup, "plants_code" for ftt_plants_sapling_lookup,
#' "common_name" for ftt_common_name_sapling_lookup.
#' @param genus For the species lookup method only, the genus to match on.
#'
#' @return The Python command used to generate renders for the given code.
#'
ftt_generic_sapling_lookup <- function(code, alias, genus = NULL) {
  if (alias == "species") stopifnot(length(code) == length(genus))
  con <- DBI::dbConnect(duckdb::duckdb(),
                        dbdir = paste0(
                          tools::R_user_dir(package = "forthetrees"),
                          "/treedb.db"
                          ),
                        read_only = TRUE
  )

  query <- switch(alias,
                  "genus" = "
SELECT
    std.render_call
FROM {table} gn
    LEFT JOIN sapling_tree_defs std
        ON COALESCE(gn.render_key, 'default') = std.render_key
WHERE gn.{lookup} = ?;
                  ",
                  "species" = "
SELECT
    std.render_call
FROM {table} sp
    LEFT JOIN genus_lookup gn ON sp.genus = gn.genus
    LEFT JOIN sapling_tree_defs std
        ON COALESCE(sp.render_key, gn.render_key, 'default') = std.render_key
WHERE
    sp.{lookup} = ?
        AND sp.genus = ?;
                  ",
                  "
SELECT
    std.render_call
FROM {table} tbl
    LEFT JOIN species_lookup sp
         ON tbl.genus = sp.genus
        AND tbl.species = sp.species
    LEFT JOIN genus_lookup gn ON sp.genus = gn.genus
    LEFT JOIN sapling_tree_defs std
        ON COALESCE(sp.render_key, gn.render_key, 'default') = std.render_key
WHERE tbl.{lookup} IN (?);
                  ")

  default_query <- "SELECT render_call FROM sapling_tree_defs WHERE render_key = 'default';" # nolint

  query <- glue::glue(
    query,
    table = paste0(alias, "_lookup"),
    lookup = alias
  )

  call_command <- vector("character", length(code))
  render_call <- DBI::dbSendQuery(con, query)

  for (i in seq_len(length(code))) {

    if (alias == "species") {
      arg_list <- list(code[[i]], genus[[i]])
    } else {
      arg_list <- list(code[[i]])
    }

    DBI::dbBind(render_call, arg_list)
    query_out <- DBI::dbFetch(render_call)

    if (is_missing(query_out[1, 1])) {
      query_out <- DBI::dbGetQuery(con, default_query)
    }

    call_command[[i]] <- query_out[1, 1]

  }

  DBI::dbClearResult(render_call)
  DBI::dbDisconnect(con, shutdown = TRUE)

  names(call_command) <- code
  call_command

}


#' Retrieve sapling rendering methods for a given lookup value.
#'
#' This function retrieves the Python command used to render given species of
#' trees, as identified by their FIA identifier, USDA PLANTS code identifier,
#' common name (as specified by the FIA program), species, or genus. Note that
#' capitalization is ignored when matching all character-based lookups -- all
#' lookup values are stored as lower case and all queries are coerced to lower
#' case. If no render method has been defined for the lookup value, a default
#' method is returned.
#'
#' @param fia_code The FIA species codes (See appendix F of Burrill et al. 2018)
#' to search for.
#' @param plants_code The USDA PLANTS species codes (USDA 2021)
#' to search for.
#' @param common_name The common names used to identify a particular species
#' (See appendix F of Burrill et al. 2018) to search for.
#' @param species The species to lookup, as a string in the format
#' `genus species` (See appendix F of Burrill et al. 2018) to search for.
#' @param genus The genera to search for.
#'
#' @return The Python command used to generate renders for the given lookup.
#'
#' @references
#' Burrill, Elizabeth A.; Wilson, Andrea M.; Turner, Jeffery A.; Pugh, Scott A.;
#' Menlove, James; Christiansen, Glenn; Conkling, Barbara L.; David, Winnie.
#' 2018. The Forest Inventory and Analysis Database: database description and
#' user guide version 8.0 for Phase 2. U.S. Department of Agriculture, Forest
#' Service. 946 p. (Online). Available at web address:
#' \url{http://www.fia.fs.fed.us/library/database-documentation}.
#'
#' USDA, NRCS. 2021. The PLANTS Database (\url{http://plants.usda.gov},
#' 14 January 2021). National Plant Data Team, Greensboro, NC 27401-4901 USA.
#'
#' @rdname sapling_lookup
#' @export
ftt_fia_sapling_lookup <- function(fia_code) {
  ftt_generic_sapling_lookup(fia_code, "fia_code")
}

#' @rdname sapling_lookup
#' @export
ftt_plants_sapling_lookup <- function(plants_code) {
  plants_code <- tolower(plants_code)
  ftt_generic_sapling_lookup(plants_code, "plants_code")
}

#' @rdname sapling_lookup
#' @export
ftt_common_name_sapling_lookup <- function(common_name) {
  common_name <- tolower(common_name)
  ftt_generic_sapling_lookup(common_name, "common_name")
}

#' @rdname sapling_lookup
#' @export
ftt_species_sapling_lookup <- function(species) {
  species <- tolower(species)
  species <- strsplit(species, " ")
  genus <- species[[1]][[1]]
  species <- paste0(utils::tail(species[[1]], -1), collapse = " ")
  ftt_generic_sapling_lookup(species, "species", genus)
}

#' @rdname sapling_lookup
#' @export
ftt_genus_sapling_lookup <- function(genus) {
  genus <- tolower(genus)
  ftt_generic_sapling_lookup(genus, "genus")
}
