# get level layer ---------------------------------------------------------------


#' Add a level to a dimension
#'
#' Add a level to a dimension. This level can then be related to other levels of
#' the dimension.
#'
#' @param gd A `geodimension` object.
#' @param level_name A string.
#' @param attributes A boolean.
#' @param surrogate_key A boolean.
#' @param inherited A boolean.
#' @param geometry A string.
#'
#' @return A `sf` object.
#'
#' @family information gathering functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#'
#' @export
get_level_layer <- function(gd,
                            level_name = NULL,
                            attributes = FALSE,
                            surrogate_key = FALSE,
                            inherited = FALSE,
                            geometry = NULL) {
  UseMethod("get_level_layer")
}


#' @rdname get_level_layer
#' @export
get_level_layer.geodimension <- function(gd,
                                         level_name = NULL,
                                         attributes = FALSE,
                                         surrogate_key = FALSE,
                                         inherited = FALSE,
                                         geometry = NULL) {
  stopifnot(level_name %in% names(gd$geolevel))
  if (is.null(geometry)) {
    geometry <- names(gd$geolevel[[level_name]]$geometry)[1]
  } else {
    stopifnot(geometry %in% names(gd$geolevel[[level_name]]$geometry))
  }
  layer <- gd$geolevel[[level_name]]$geometry[[geometry]]
  if (attributes) {
    data <- gd %>%
      get_level_data(level_name = level_name, inherited = inherited)
    if (surrogate_key) {
      sel <- NULL
    } else {
      sel <- c(1)
    }
    layer <- data %>%
      dplyr::left_join(layer, by = names(data)[1]) %>%
      dplyr::select(!sel) %>%
      sf::st_as_sf()
  }
  layer
}


