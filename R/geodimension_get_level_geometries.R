# get level geometries ---------------------------------------------------------------

#' Get level geometries
#'
#' Gets the geometry types defined for a given level.
#'
#' @param gd A `geodimension` object.
#' @param level_name A string.
#'
#' @return A vector of names.
#'
#' @family information output functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' lg <- gd_us %>%
#'   get_level_geometries(level_name = "state")
#'
#' @export
get_level_geometries <- function(gd,
                                 level_name = NULL) {
  UseMethod("get_level_geometries")
}


#' @rdname get_level_geometries
#' @export
get_level_geometries.geodimension <- function(gd,
                                              level_name = NULL) {
  stopifnot(level_name %in% names(gd$geolevel))
  sort(names(gd$geolevel[[level_name]]$geometry))
}

