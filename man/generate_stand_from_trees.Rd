% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_stand_from_trees.R
\name{generate_stand_from_trees}
\alias{generate_stand_from_trees}
\title{Generate stem maps from tables of individual trees}
\usage{
generate_stand_from_trees(
  data,
  stand_area,
  ...,
  measured_area,
  cols = dplyr::everything()
)
}
\arguments{
\item{data}{The data.frame}

\item{stand_area}{sf object: The polygon to simulate trees within. Sampling
is perfomed using \code{\link[sf:st_sample]{sf::st_sample()}}.}

\item{...}{Passed to \code{\link[sf:st_sample]{sf::st_sample()}}.}

\item{measured_area}{Either numeric (the total amount of area represented by
\code{data}) of sf object (representing the measurement boundary). Used to
calculate}

\item{cols}{Names of columns to simulate for each new tree}
}
\value{
An sf object, representing new tree positions in the stem map, with simulated variables.
}
\description{
This function simulates stem maps for stands, basing density and attributes
on tree measurements provided to \code{data}.
}
\examples{
stand_area <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 140, ymax = 140))
stand_area <- sf::st_as_sfc(stand_area)

measured_area <- 19600

trees <- tree_positions[c("SPECIES", "DBH85")]
trees$SPECIES <- factor(trees$SPECIES)

generate_stand_from_trees(
  trees,
  stand_area,
  measured_area = measured_area
)

}
