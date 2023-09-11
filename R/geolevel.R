
#' `geolevel` S3 class
#'
#' A `geolevel` object is created from a given geographic layer. The attributes
#' of the layer to be included in the level can be indicated, and the subset of
#' these that make up the natural key. If no attribute is indicated, all are
#' considered. In any case, the attributes that make up the key must be
#' indicated.
#'
#' A level can have several associated geometries (point, polygon or line). The
#' geometry is obtained from the layer data.
#'
#' The name of the level is used later to reference it and relate it to other
#' levels.
#'
#' @param name A string, level name.
#' @param layer A `sf` object.
#' @param attributes A vector, selected attributes.
#' @param key A vector, attributes that compose the key.
#'
#' @return A `geolevel` object.
#'
#' @family geolevel definition functions
#'
#' @examples
#' region <-
#'   geolevel(name = "region",
#'            layer = layer_us_region,
#'            key = c("geoid"))
#'
#' @importFrom rlang :=
#'
#' @export
geolevel <-
  function(name = NULL,
           layer = NULL,
           attributes = NULL,
           key = NULL) {
    stopifnot("Missing geolevel name." = !is.null(name))
    stopifnot("layer does not include sf object." = methods::is(layer, "sf"))
    geometry <- get_geometry(layer)
    if (!(geometry %in% c("polygon", "point", "line"))) {
      stop(sprintf('layer has unsupported geometry: %s.', geometry[1]))
    }

    data <- tibble::tibble((sf::st_drop_geometry(layer)))
    attributes <- validate_attributes(names(data), attributes)

    stopifnot("The key is missing." = !is.null(key))
    key <- validate_attributes(names(data), key)
    attributes <- unique(c(key, attributes))

    data <- data |>
      dplyr::select(tidyselect::all_of(attributes)) |>
      dplyr::group_by_at(attributes) |>
      dplyr::summarize(.groups = "drop")

    data_key <- data |>
      dplyr::select(tidyselect::all_of(key)) |>
      dplyr::group_by_at(key) |>
      dplyr::summarize(.groups = "drop")

    is_a_valid_key <- (nrow(data) == nrow(data_key))
    stopifnot("The key is invalid." = nrow(data) == nrow(data_key))

    surrogate_key <- surrogate_key_name(name)
    data_key <- data_key |>
      tibble::add_column(!!surrogate_key := 1:nrow(data_key), .before = 1)

    data <- data_key |>
      dplyr::left_join(data, by = key)

    layer <- layer |>
      dplyr::select(tidyselect::all_of(key)) |>
      dplyr::group_by_at(key) |>
      dplyr::summarize(.groups = "drop")

    # only surrogate key and geometry
    layer <- data_key |>
      dplyr::left_join(layer, by = key) |>
      sf::st_as_sf() |>
      dplyr::select(tidyselect::all_of(names(data_key)[1]))

    # only instances with geometry
    layer <- layer[!is.na(sf::st_dimension(layer)),]

    geolevel <- list(data = data,
                     geometry = list(geometry = layer))
    names(geolevel$geometry) <- geometry

    structure(
      geolevel,
      name = name,
      attributes = attributes,
      key = key,
      surrogate_key = surrogate_key,
      n_instances_data = nrow(data),
      class = "geolevel"
    )
  }


#' Add geometry to a level
#'
#' A level can have several geometries (*point*, *polygon* or *line*). This
#' function adds the geometry of the layer to the level.
#'
#' The association of the geometry to the existing instances is done through
#' join using the level and layer keys.
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
#' @family geolevel definition functions
#'
#' @examples
#' us_state_point <-
#'   coordinates_to_geometry(layer_us_state,
#'                           lon_lat = c("intptlon", "intptlat"))
#'
#' state <-
#'   geolevel(name = "state",
#'            layer = layer_us_state,
#'            key = c("geoid")) |>
#'   add_geometry(layer = us_state_point)
#'
#' @export
add_geometry <- function(gl, layer, layer_key, level_key) UseMethod("add_geometry")


#' @rdname add_geometry
#' @export
add_geometry.geolevel <- function(gl,
                                  layer = NULL,
                                  layer_key = NULL,
                                  level_key = NULL) {
  stopifnot("layer does not include sf object." = methods::is(layer, "sf"))
  geometry <- get_geometry(layer)
  if (!(geometry %in% c("polygon", "point", "line"))) {
    stop(sprintf('layer has unsupported geometry: %s.', geometry[1]))
  }
  stopifnot("This geometry type is already defined for the layer." = !(geometry %in% names(gl$geometry)))
  if (is.null(level_key)) {
    level_key <- attr(gl, "key")
  } else {
    level_key <- validate_attributes(names(gl$data), level_key)
    stopifnot("level_key is not a key o the level." = nrow(gl$data) == nrow(unique(gl$data[, level_key])))
  }
  if (is.null(layer_key)) {
    layer_key <- level_key
  } else {
    stopifnot("Keys are not the same length." = length(unique(layer_key)) == length(level_key))
  }
  layer_key <- validate_attributes(names(layer), layer_key)

  layer <- layer |>
    dplyr::select(tidyselect::all_of(layer_key)) |>
    dplyr::group_by_at(layer_key) |>
    dplyr::summarize(.groups = "drop")
  # only the layer_key and geometry
  names_layer <- names(layer)
  names(layer) <- c(level_key, names_layer[length(names_layer)])

  layer <- gl$data |>
    dplyr::select(tidyselect::all_of(c(attr(gl, "surrogate_key"), level_key))) |>
    dplyr::left_join(layer, by = level_key) |>
    sf::st_as_sf() |>
    dplyr::select(attr(gl, "surrogate_key"))

  # only instances with geometry
  layer <- layer[!is.na(sf::st_dimension(layer)),]

  gl$geometry[[geometry]] <- layer
  gl
}


# -----------------------------------------------------------------------

#' Validate attribute names
#'
#' @param defined_attributes A vector of strings, defined attribute names.
#' @param attributes A vector of strings, new attribute names.
#' @param repeated A boolean, repeated attributes allowed.
#'
#' @return A vector of strings, attribute names.
#'
#' @keywords internal
validate_attributes <- function(defined_attributes, attributes, repeated = FALSE) {
  if (is.null(attributes)) {
    attributes <- defined_attributes
  } else {
    if (!repeated) {
      stopifnot("There are repeated attributes." = length(attributes) == length(unique(attributes)))
    }
    for (attribute in attributes) {
      if (!(attribute %in% defined_attributes)) {
        stop(sprintf(
          "'%s' is not defined as attribute.",
          attribute
        ))
      }
    }
  }
  attributes
}

# -----------------------------------------------------------------------

#' Surrogate key name
#'
#' @param name A string.
#'
#' @return A string.
#'
#' @keywords internal
surrogate_key_name <- function(name) {
  sprintf("%s_key", snakecase::to_snake_case(name))
}

