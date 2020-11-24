
#' Add a level to a dimension
#'
#' Once a level is part of the dimension, it can then be related to other levels
#' of the dimension.
#'
#' @param gd A `geodimension` object.
#' @param level A `geolevel`, level to add to the dimension.
#'
#' @return A `geodimension`.
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
#' division <-
#'   geolevel(name = "division",
#'            layer = layer_us_division,
#'            key = c("geoid"))
#'
#' gd <-
#'   geodimension(name = "gd_us",
#'                level = region) %>%
#'   add_level(division)
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
