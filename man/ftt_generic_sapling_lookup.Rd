% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/key_lookups.R
\name{ftt_generic_sapling_lookup}
\alias{ftt_generic_sapling_lookup}
\title{Internal function providing key:value matching for render commands}
\usage{
ftt_generic_sapling_lookup(code, alias)
}
\arguments{
\item{code}{The code to look up -- FIA code, PLANTS shortcode, or common name}

\item{alias}{A string indicating the calling alias function -- "fia_code" for
ftt_fia_sapling_lookup, "plants_code" for ftt_plants_sapling_lookup,
"common_name" for ftt_common_name_sapling_lookup,
"genus" for ftt_genus_sapling_lookup, or "species" for
ftt_species_sapling_lookup.}

\item{genus}{For the species lookup method only, the genus to match on.}
}
\value{
The Python command used to generate renders for the given code.
}
\description{
Internal function providing key:value matching for render commands
}
