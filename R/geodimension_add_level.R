
#' Add a level to a dimension
#'
#' Add a level to a dimension. This level can then be related to other levels of
#' the dimension.
#'
#' @param gd A `geodimension` object.
#' @param level A `geolevel`, level to add to the dimension.
#'
#' @return A `geodimension`.
#'
#' @family geodimension functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#'
#' @export
add_level <- function(gd,
                      level = NULL) {
  UseMethod("add_level")
}


#' @rdname add_level
#' @export
add_level.geodimension <- function(gd,
                                   level = NULL) {
  stopifnot(!(attr(level, "name") %in% names(gd$geolevel)))
  gd$geolevel[[attr(level, "name")]] <- level

  data <- level$data[1]
  names(data) <- attr(level, "name")
  gd$relation[[attr(level, "name")]] <- data
  gd
}
