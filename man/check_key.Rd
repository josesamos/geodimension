% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/common.R
\name{check_key}
\alias{check_key}
\title{Check key}
\usage{
check_key(table, key = NULL)
}
\arguments{
\item{table}{A \code{tibble} object.}

\item{key}{A vector, attributes that compose the key.}
}
\value{
A boolean.
}
\description{
Check if the specified set of attributes can be the key of the table.
}
\details{
The table can be a data table or a vector layer.
}
\examples{
library(sf)

is_key <- check_key(layer_us_region, key = c("name"))

}
\seealso{


Other level definition functions: 
\code{\link{add_geometry}()},
\code{\link{complete_point_geometry}()},
\code{\link{coordinates_to_geometry}()},
\code{\link{geolevel}()},
\code{\link{get_empty_geometry_instances}()},
\code{\link{get_geometry}()}
}
\concept{level definition functions}
