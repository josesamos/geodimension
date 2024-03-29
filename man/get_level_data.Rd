% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explotation.R
\name{get_level_data}
\alias{get_level_data}
\alias{get_level_data.geodimension}
\title{Get level data}
\usage{
get_level_data(gd, level_name, inherited, add_prefix)

\method{get_level_data}{geodimension}(gd, level_name = NULL, inherited = FALSE, add_prefix = TRUE)
}
\arguments{
\item{gd}{A \code{geodimension} object.}

\item{level_name}{A string.}

\item{inherited}{A boolean.}

\item{add_prefix}{A boolean.}
}
\value{
A \code{tibble} object.
}
\description{
Get the data table of a given level.
}
\details{
It allows selecting whether we want only the data defined locally in the level
or also those that it inherits from other higher levels with which it is related.

In case of inheriting attributes from other levels, in the table, these can
have as a prefix the name of the level.
}
\examples{

ld <- gd_us |>
  get_level_data(level_name = "county",
                 inherited = TRUE)

}
\seealso{
\code{\link{geodimension}}, \code{\link{geolevel}}

Other information gathering functions: 
\code{\link{get_higher_level_names}()},
\code{\link{get_level_data_geo}()},
\code{\link{get_level_geometries}()},
\code{\link{get_level_layer}()},
\code{\link{get_level_names}()}
}
\concept{information gathering functions}
