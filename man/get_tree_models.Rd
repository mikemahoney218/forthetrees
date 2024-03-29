% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_tree_models.R
\name{get_tree_models}
\alias{get_tree_models}
\title{Download 3D tree models for Unity}
\usage{
get_tree_models(directory = NULL)
}
\arguments{
\item{directory}{Optionally, the directory to extract the downloaded models
in. If NULL, the default, saves to \code{tools::R_user_dir("forthetrees")}.}
}
\description{
This is a simple helper function downloading the tree models stored at
https://github.com/mikemahoney218/tree_models .
}
