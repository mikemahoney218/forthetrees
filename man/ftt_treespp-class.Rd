% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\docType{class}
\name{ftt_treespp-class}
\alias{ftt_treespp-class}
\title{S4 class for tree species: ID codes and keys to look up rendering methods.}
\description{
S4 class for tree species: ID codes and keys to look up rendering methods.
}
\section{Slots}{

\describe{
\item{\code{id_code}}{The code indicating what sort of identifier is stored in id_key
-- one of fia_code, plants_code, common_name, genus, or species.}

\item{\code{id_key}}{The key identifying the tree species in the schema referenced
by id_code.}

\item{\code{idx}}{Character: a unique identifier (or "index") for each object to be
modeled. Must be unique with no \code{NA} or \code{NULL} values, but otherwise is
not validated.}

\item{\code{x,y,z}}{Numeric: distance of the origin of the object from the origin of
the grid system (the central point at 0, 0, 0) in meters in the given
direction. Must have no \code{NA}, \code{NULL}, \code{NaN}, \code{Inf}, or \code{-Inf} values.}

\item{\code{metadata}}{Data frame: a table containing additional information on the
objects to be modeled. Optional, but if this slot is used then the data frame
must contain a column named \code{idx} which should correspond to the \code{idx} slot.
Only the existence of this column is validated.}

\item{\code{appendix}}{List: additional data produced in the generation of the
object. Not validated; any additional outputs that don't map to modeled
objects may be inserted here.}
}}

\seealso{
Other classes and related functions: 
\code{\link{ftt_treesize-class}}
}
\concept{classes and related functions}
