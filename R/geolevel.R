
#' `geolevel` S3 class
#'
#' A `geolevel` object is created from a given geographic layer. The attributes
#' of the layer to be included in the level can be indicated; if no attribute is
#' indicated, all are considered. The attributes that make up the key must be
#' indicated.
#'
#' A level can have two associated geometries (point or polygon). The geometry
#' is obtained from the layer data.
#'
#' We can also define a level from a `tibble`, which does not have any associated
#' geometry. The geometry will be obtained from the relationships between levels
#' that we define or from layers related to this data.
#'
#' The name of the level is used later to reference it and relate it to other
#' levels.
#'
#' @param name A string, level name.
#' @param layer A `tibble` or `sf` object.
#' @param attributes A string vector, selected attributes.
#' @param key A string vector, attributes that compose the key.
#' @param snake_case A boolean, transform all names to snake_case.
#'
#' @return A `geolevel` object.
#'
#' @family geolevel definition functions
#'
#' @examples
#' region <-
#'   geolevel(name = "region",
#'            layer = layer_us_region,
#'            key = "geoid",
#'            snake_case = TRUE)
#'
#' @export
geolevel <-
  function(name = NULL,
           layer = NULL,
           attributes = NULL,
           key = NULL,
           snake_case = FALSE) {
    stopifnot("Missing geolevel name." = !is.null(name))
    if (methods::is(layer, "sf")) {
      geometry <- get_geometry(layer)
      if (!(geometry %in% c("polygon", "point"))) {
        stop(sprintf('layer has unsupported geometry: %s.', geometry[1]))
      }
    } else {
      geometry <- NULL
    }
    if (snake_case) {
      name <- snakecase::to_snake_case(name)
      if (!is.null(attributes)) {
        attributes <- snakecase::to_snake_case(attributes)
      }
      if (!is.null(key)) {
        key <- snakecase::to_snake_case(key)
      }
      names(layer) <- snakecase::to_snake_case(names(layer))
    }

    if (!is.null(geometry)) {
      data <- tibble::tibble((sf::st_drop_geometry(layer)))
    } else {
      data <- layer
    }
    attributes <- validate_names(names(data), attributes, 'attribute')

    stopifnot("The key is missing." = !is.null(key))
    key <- validate_names(names(data), key, 'attribute')
    attributes <- unique(c(key, attributes))

    data <- data |>
      dplyr::select(tidyselect::all_of(attributes)) |>
      dplyr::group_by_at(attributes) |>
      dplyr::summarize(.groups = "drop")

    data_key <- data |>
      dplyr::select(tidyselect::all_of(key)) |>
      dplyr::group_by_at(key) |>
      dplyr::summarize(.groups = "drop")

    stopifnot("The key is invalid." = nrow(data) == nrow(data_key))

    if (!is.null(geometry)) {
      layer <- layer |>
        dplyr::select(tidyselect::all_of(key))
      if (nrow(layer) > nrow(data_key)) {
        layer <- layer |>
          dplyr::group_by_at(key) |>
          dplyr::summarize(.groups = "drop")
      }
      # only instances with geometry
      layer <- layer[!is.na(sf::st_dimension(layer)),]
      geo <- list(geometry = layer)
    } else {
      geo <- list()
    }

    geolevel <- list(name = name,
                     key = key,
                     snake_case = snake_case,
                     data = data,
                     geometry = geo)
    names(geolevel$geometry) <- geometry

    structure(
      geolevel,
      class = "geolevel"
    )
  }


#' Add geometry to a level
#'
#' A level can have several geometries (*point* or *polygon*). This function adds
#' the geometry of the layer to the level.
#'
#' The association of the geometry to the existing instances is done through join
#' using the level and layer keys.
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
#'            key = "geoid",
#'            snake_case = TRUE) |>
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
  if (!(geometry %in% c("polygon", "point"))) {
    stop(sprintf('`layer` has unsupported geometry: %s.', geometry[1]))
  }
  stopifnot("This geometry type is already defined for the layer." = !(geometry %in% names(gl$geometry)))
  if (is.null(level_key)) {
    level_key <- gl$key
  } else {
    if (gl$snake_case) {
      level_key <- snakecase::to_snake_case(level_key)
    }
    level_key <- validate_names(names(gl$data), level_key, 'attribute')
    stopifnot("`level_key` is not a key of the level." = nrow(gl$data) == nrow(unique(gl$data[, level_key])))
  }
  if (is.null(layer_key)) {
    layer_key <- level_key
  } else {
    if (gl$snake_case) {
      layer_key <- snakecase::to_snake_case(layer_key)
    }
    stopifnot("Keys are not the same length." = length(unique(layer_key)) == length(level_key))
  }
  if (gl$snake_case) {
    names(layer) <- snakecase::to_snake_case(names(layer))
  }
  layer_key <- validate_names(names(layer), layer_key, 'attribute')

  layer <- layer |>
    dplyr::select(tidyselect::all_of(layer_key)) |>
    dplyr::group_by_at(layer_key) |>
    dplyr::summarize(.groups = "drop")
  # only the layer_key and geometry
  names_layer <- names(layer)
  names(layer) <- c(level_key, names_layer[length(names_layer)])

  layer <- gl$data |>
    dplyr::select(tidyselect::all_of(unique(c(gl$key, level_key)))) |>
    dplyr::left_join(layer, by = level_key) |>
    sf::st_as_sf() |>
    dplyr::select(gl$key)

  # only instances with geometry
  layer <- layer[!is.na(sf::st_dimension(layer)),]

  gl$geometry[[geometry]] <- layer
  gl
}


# -----------------------------------------------------------------------

#' snake case geolevel
#'
#' @param gl A `geolevel` object.
#'
#' @return A `geolevel` object.
#'
#' @keywords internal
snake_case_geolevel <- function(gl) {
  gl$name <- snakecase::to_snake_case(gl$name)
  gl$key <- snakecase::to_snake_case(gl$key)
  gl$snake_case <- TRUE
  names(gl$data) <- snakecase::to_snake_case(names(gl$data))
  for (i in names(gl$geometry)) {
    names(gl$geometry[[i]]) <- snakecase::to_snake_case(names(gl$geometry[[i]]))
  }
  gl
}

# empty geometry ----------------------------------------------------------

#' Get empty geometry instances
#'
#' Get the instances of the data table that do not have associated geometry for
#' the specified geometry type.
#'
#' @param gl A `geolevel` object.
#' @param geometry A string, type of geometry of the layer.
#'
#' @return A `tibble`.
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
#'            key = "geoid",
#'            snake_case = TRUE) |>
#'   add_geometry(layer = us_state_point)
#'
#' empty_geometry_instances <- state |>
#'   get_empty_geometry_instances(geometry = "point")
#'
#' @export
get_empty_geometry_instances <- function(gl, geometry) {
  UseMethod("get_empty_geometry_instances")
}


#' @rdname get_empty_geometry_instances
#' @export
get_empty_geometry_instances.geolevel <- function(gl, geometry = NULL) {
  if (is.null(geometry)) {
    geometry <- names(gl$geometry)[1]
  } else {
    stopifnot("This geometry is not included in the geolevel." = geometry %in% names(gl$geometry))
  }
  layer_data <- tibble::tibble((sf::st_drop_geometry(gl$geometry[[geometry]])))

  empty <- dplyr::setdiff(gl$data[, gl$key], layer_data) |>
     dplyr::inner_join(gl$data, by = gl$key)

  empty
}

# complete point geometry ----------------------------------------------------------

#' Complete point geometry
#'
#' In case of having the polygon geometry defined, it obtains the point geometry
#' from it.
#'
#' If the point geometry was already defined, if there are instances with this
#' geometry empty, it completes them.
#'
#'
#' @param gl A `geolevel` object.
#'
#' @return A `geolevel` object.
#'
#' @family geolevel definition functions
#'
#' @examples
#' state <-
#'   geolevel(name = "state",
#'            layer = layer_us_state,
#'            key = "geoid",
#'            snake_case = TRUE) |>
#'   complete_point_geometry()
#'
#' @export
complete_point_geometry <- function(gl) {
  UseMethod("complete_point_geometry")
}

#' @rdname complete_point_geometry
#' @export
complete_point_geometry.geolevel <- function(gl) {
  stopifnot("polygon" %in% names(gl$geometry))
  layer <- gl$geometry[["polygon"]]
  # suppress warning message
  sf::st_agr(layer) = "constant"
  crs <- sf::st_crs(layer)
  layer <-
    sf::st_transform(layer, 3857) |>
    sf::st_point_on_surface() |>
    sf::st_transform(crs)
  if ("point" %in% names(gl$geometry)) {
    empty <- get_empty_geometry_instances(gl, "point")
    if (nrow(empty) > 0) {
      empty <-  empty |>
        dplyr::inner_join(layer, by = gl$key) |>
        sf::st_as_sf() |>
        dplyr::select(gl$key) |>
        sf::st_transform(sf::st_crs(gl$geometry[["point"]]))
      gl$geometry[["point"]] <- rbind(gl$geometry[["point"]], empty)
    }
  } else {
    gl$geometry[["point"]] <- layer
  }
  gl
}
