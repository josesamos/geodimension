# select levels ---------------------------------------------------------

#' Add a level to a dimension
#'
#' Add a level to a dimension. This level can then be related to other levels of
#' the dimension.
#'
#' @param gd A `geodimension` object.
#' @param level_names A vector of names.
#'
#' @return A `geodimension` object.
#'
#' @family configuration functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#'
#' @export
select_levels <- function(gd, level_names = NULL) {
  UseMethod("select_levels")
}


#' @rdname select_levels
#' @export
select_levels.geodimension <- function(gd, level_names = NULL) {
  level_names <- unique(level_names)
  existing_names <- names(gd$geolevel)
  stopifnot(level_names %in% existing_names)
  delete <- generics::setdiff(existing_names, level_names)
  for (del in delete) {
    gd$geolevel[[del]] <- NULL
    gd$relation[[del]] <- NULL
  }
  for (level in names(gd$relation)) {
    rel_names <- generics::setdiff(names(gd$relation[[level]]), delete)
    gd$relation[[level]] <- gd$relation[[level]][, rel_names]
  }
  gd
}


# transform crs ---------------------------------------------------------

#' Add a level to a dimension
#'
#' Add a level to a dimension. This level can then be related to other levels of
#' the dimension.
#'
#' @param gd A `geodimension` object.
#' @param crs A coordinate reference system: integer with the EPSG code, or
#'   character with proj4string.
#'
#' @return A `geodimension`.
#'
#' @family configuration functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#'
#' @export
transform_crs <- function(gd,
                          crs = NULL) {
  UseMethod("transform_crs")
}

#' @rdname transform_crs
#' @export
transform_crs.geodimension <- function(gd,
                                       crs = NULL) {
  for (layer in names(gd$geolevel)) {
    for (geom in names(gd$geolevel[[layer]]$geometry)) {
      gd$geolevel[[layer]]$geometry[[geom]] <-
        gd$geolevel[[layer]]$geometry[[geom]] %>%
        sf::st_transform(crs = crs)
    }
  }
  gd
}
