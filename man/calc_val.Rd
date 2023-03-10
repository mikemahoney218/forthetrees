% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz.R
\name{calc_val}
\alias{calc_val}
\title{Standard way to replace & replicate values for class-creation functions}
\usage{
calc_val(vec, length_out, replace_val = 0)
}
\arguments{
\item{vec}{The vector to replace and replicate values throughout}

\item{length_out}{The target length of the vector}

\item{replace_val}{The value to replace missing values with}
}
\value{
A vector of length \code{length_out} with missing values replaced by
\code{replace_val}
}
\description{
Standard way to replace & replicate values for class-creation functions
}
\keyword{internal}
