
#' Add geometry to a level
#'
#' A level can have several associated geometries (point, polygon or line). Add
#' the geometry of the layer or replace an existing one of the indicated type.
#'
#' @param gl A `geolevel` object.
#' @param layer A `sf` object.
#' @param key A vector of string.
#' @param level_key A vector of string.
#'
#' @return A `geolevel`.
#'
#' @family geodimension functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#'
#' @export
add_geometry <- function(gl,
                         layer = NULL,
                         key = NULL,
                         level_key = NULL) {
  UseMethod("add_geometry")
}


#' @rdname add_geometry
#' @export
add_geometry.geolevel <- function(gl,
                                  layer = NULL,
                                  key = NULL,
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
  if (is.null(key)) {
    key <- level_key
  } else {
    key <- unique(key)
    stopifnot(length(key) == length(level_key))
  }
  stopifnot(key %in% names(layer))

  layer <- layer %>%
    dplyr::select(tidyselect::all_of(key)) %>%
    dplyr::group_by_at(key) %>%
    dplyr::summarize(.groups = "drop")
  # only the key and geometry
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

# empty geometry ----------------------------------------------------------

#' get empty geometry
#'
#' A level can have several associated geometries (point, polygon or line). Add
#' the geometry of the layer or replace an existing one of the indicated type.
#'
#' @param gl A `geolevel` object.
#' @param geometry A string, type of geometry of the layer.
#'
#' @return A `tibble`.
#'
#' @family geodimension functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#'
#' @export
get_empty_geometry_instances <- function(gl,
                         geometry = NULL) {
  UseMethod("get_empty_geometry_instances")
}


#' @rdname get_empty_geometry_instances
#' @export
get_empty_geometry_instances.geolevel <- function(gl,
                                  geometry = NULL) {
  stopifnot(geometry %in% names(gl$geometry))
  if (is.null(geometry)) {
    geometry <- names(gl$geometry)[1]
  }
  gl$data[!(gl$data[[1]] %in% gl$geometry[[geometry]][[1]]), ]
}


# complete point geometry ----------------------------------------------------------

#' complete point geometry
#'
#' A level can have several associated geometries (point, polygon or line). Add
#' the geometry of the layer or replace an existing one of the indicated type.
#'
#' @param gl A `geolevel` object.
#'
#' @return A `geolevel` object.
#'
#' @family geodimension functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#'
#' @export
complete_point_geometry <- function(gl) {
  UseMethod("complete_point_geometry")
}


#' @rdname complete_point_geometry
#' @export
complete_point_geometry.geolevel <- function(gl) {
  stopifnot("polygon" %in% names(gl$geometry))
  if ("point" %in% names(gl$geometry)) {
    layer <- gl$geometry[["polygon"]][!(gl$data[[1]] %in% gl$geometry[["point"]][[1]]), ]
    # to avoid warning: make the assumption (that the attribute is constant throughout the geometry)
    sf::st_agr(layer) = "constant"
    gl$geometry[["point"]] <- gl$geometry[["point"]] %>%
      tibble::add_row(sf::st_point_on_surface(layer))
  } else {
    layer <- gl$geometry[["polygon"]]
    # to avoid warning: make the assumption (that the attribute is constant throughout the geometry)
    sf::st_agr(layer) = "constant"
    gl$geometry[["point"]] <- sf::st_point_on_surface(layer)
  }
  gl
}

