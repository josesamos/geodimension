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
#' A `geodimension` object is created. In addition to the name of the dimension,
#' the lowest and highest level of the dimension are indicated.
#'
#' @inheritParams new_geodimension
#'
#' @return A `geodimension` object.
#'
#' @family geodimension functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#'
#' @export
geodimension <- function(name = NULL,
                         level = NULL) {
  new_geodimension(name, level)
}


