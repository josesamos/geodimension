
#' Add geometry to a level
#'
#' A level can have several geometries (*point*, *polygon* or *line*). This
#' function adds the geometry of the layer to the level.
#'
#' The association of the geometry to the existing instances is done through
#' join using the level key and the layer key.
#'
#' If none is indicated, by default the key defined in the level is considered.
#'
#' @param gl A `geolevel` object.
#' @param layer A `sf` object.
#' @param layer_key A vector of string.
#' @param level_key A vector of string.
#'
#' @return A `geolevel`.
#'
#' @family level definition functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#' library(sf)
#'
#' us_state_point <-
#'   coordinates_to_geometry(layer_us_state,
#'                           lon_lat = c("intptlon", "intptlat"))
#'
#' state <-
#'   geolevel(name = "state",
#'            layer = layer_us_state,
#'            key = c("geoid")) %>%
#'   add_geometry(layer = us_state_point)
#'
#' @export
add_geometry <- function(gl,
                         layer = NULL,
                         layer_key = NULL,
                         level_key = NULL) {
  UseMethod("add_geometry")
}


#' @rdname add_geometry
#' @export
add_geometry.geolevel <- function(gl,
                                  layer = NULL,
                                  layer_key = NULL,
                                  level_key = NULL) {
  geometry <- get_geometry(layer)
  stopifnot(geometry %in% c("polygon", "point", "line"))
  stopifnot(!(geometry %in% names(gl$geometry)))
  if (is.null(level_key)) {
    level_key <- attr(gl, "key")
  } else {
    level_key <- unique(level_key)
    level_key_is_a_key <- (nrow(gl$data) == nrow(unique(gl$data[, level_key])))
    stopifnot(level_key_is_a_key)
  }
  if (is.null(layer_key)) {
    layer_key <- level_key
  } else {
    layer_key <- unique(layer_key)
    stopifnot(length(layer_key) == length(level_key))
  }
  stopifnot(layer_key %in% names(layer))

  layer <- layer %>%
    dplyr::select(tidyselect::all_of(layer_key)) %>%
    dplyr::group_by_at(layer_key) %>%
    dplyr::summarize(.groups = "drop")
  # only the layer_key and geometry
  names_layer <- names(layer)
  names(layer) <- c(level_key, names_layer[length(names_layer)])

  layer <- gl$data %>%
    dplyr::select(tidyselect::all_of(c(attr(gl, "surrogate_key"), level_key))) %>%
    dplyr::left_join(layer, by = level_key) %>%
    sf::st_as_sf() %>%
    dplyr::select(attr(gl, "surrogate_key"))

  # only instances with geometry
  layer <- layer[!is.na(sf::st_dimension(layer)), ]

  gl$geometry[[geometry]] <- layer

  gl
}
