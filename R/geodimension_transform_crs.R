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
