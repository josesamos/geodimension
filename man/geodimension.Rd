% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geodimension.R
\name{geodimension}
\alias{geodimension}
\title{\code{geodimension} S3 class}
\usage{
geodimension(name = NULL, level = NULL, snake_case = FALSE)
}
\arguments{
\item{name}{A string, name of the dimension.}

\item{level}{A \code{geolevel}.}

\item{snake_case}{A boolean, transform all names to snake_case.}
}
\value{
A \code{geodimension} object.
}
\description{
A \code{geodimension} object is created. A \code{geodimension} allows us to relate
levels. In addition to the name of the \code{geodimension} , a \code{level} has to be
given.
}
\examples{

layer_us_place <- get_level_layer(gd_us, "place")

place <-
  geolevel(name = "place",
           layer = layer_us_place,
           key = "geoid")
gd <-
  geodimension(name = "gd_us",
               level = place)

}
\seealso{
\code{\link{geolevel}}, \code{\link{relate_levels}}, \code{\link{get_level_layer}}

Other geodimension definition functions: 
\code{\link{add_level}()},
\code{\link{set_level_data}()},
\code{\link{transform_crs}()}
}
\concept{geodimension definition functions}
