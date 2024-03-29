% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/relations.R
\name{get_level_keys}
\alias{get_level_keys}
\title{get level keys}
\usage{
get_level_keys(gd, level_name = NULL)
}
\arguments{
\item{gd}{A \code{geolevel} object.}

\item{level_name}{A string, name of the level.}
}
\value{
A \code{tibble} object.
}
\description{
Starting from a level, the keys of all the levels above it.
}
\keyword{internal}
