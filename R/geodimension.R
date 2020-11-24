#' `geodimension` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @param name A string, name of the dimension.
#' @param level A `geolevel`.
#'
#' @return A `geodimension` object.
#'
#' @keywords internal
new_geodimension <-
  function(name = NULL,
           level = NULL) {

    geolevel <- list()
    geolevel[[attr(level, "name")]] <- level

    relation <- list()
    data <- level$data[1]
    names(data) <- attr(level, "name")
    relation[[attr(level, "name")]] <- data

    geodimension <- list(geolevel = geolevel, relation = relation)

    structure(
      geodimension,
      name = name,
      class = "geodimension"
    )
  }

#' `geodimension` S3 class
#'
#' A `geodimension` object is created. A `geodimension` allows you to relate
#' levels. In addition to the name of the `geodimension` , a `level` has to be
#' given.
#'
#' @inheritParams new_geodimension
#'
#' @return A `geodimension` object.
#'
#' @family level association functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#' library(sf)
#'
#' region <-
#'   geolevel(name = "region",
#'            layer = layer_us_region,
#'            key = c("geoid"))
#'
#' gd <-
#'   geodimension(name = "gd_us",
#'                level = region)
#'
#' @export
geodimension <- function(name = NULL,
                         level = NULL) {
  new_geodimension(name, level)
}


