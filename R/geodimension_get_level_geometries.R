# get level geometries ---------------------------------------------------------------


#' Add a level to a dimension
#'
#' Add a level to a dimension. This level can then be related to other levels of
#' the dimension.
#'
#' @param gd A `geodimension` object.
#' @param level_name A string.
#'
#' @return A vector of names.
#'
#' @family information gathering functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
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
  names(gd$geolevel[[level_name]]$geometry)
}

