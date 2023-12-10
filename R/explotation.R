
#' Get higher level names
#'
#' Get the names of levels included in the `geodimension` that are related to the
#' given level and are upper levels. We can get only the direct levels or the
#' levels reached by passing through other levels.
#'
#' @param gd A `geodimension` object.
#' @param level_name A string.
#' @param indirect_levels A boolean.
#'
#' @return A vector of names.
#'
#' @family information gathering functions
#' @seealso \code{\link{geodimension}}, \code{\link{geolevel}}
#'
#' @examples
#'
#' ln_1 <- gd_us |>
#'   get_higher_level_names(level_name = "place")
#'
#' ln_2 <- gd_us |>
#'   get_higher_level_names(level_name = "place", indirect_levels = TRUE)
#'
#' @export
get_higher_level_names <- function(gd,
                                   level_name,
                                   indirect_levels) {
  UseMethod("get_higher_level_names")
}


#' @rdname get_higher_level_names
#' @export
get_higher_level_names.geodimension <- function(gd,
                                                level_name = NULL,
                                                indirect_levels = FALSE) {
  stopifnot("Missing level name." = !is.null(level_name))
  if (gd$snake_case) {
    level_name <- my_to_snake_case(level_name)
  }
  level_name <-
    validate_names(names(gd$geolevel), level_name, 'level')
  res <- names(gd$relation[[level_name]])
  if (indirect_levels) {
    for (l in res) {
      r <- get_higher_level_names(gd, level_name = l, indirect_levels)
      res <- union(res, r)
    }
  }
  res
}


#' Get level geometries
#'
#' Gets the geometry types defined for a given level.
#'
#' @param gd A `geodimension` object.
#' @param level_name A string.
#'
#' @return A vector of names.
#'
#' @family information gathering functions
#' @seealso \code{\link{geodimension}}, \code{\link{geolevel}}
#'
#' @examples
#'
#' lg <- gd_us |>
#'   get_level_geometries(level_name = "state")
#'
#' @export
get_level_geometries <- function(gd,
                                 level_name) {
  UseMethod("get_level_geometries")
}

#' @rdname get_level_geometries
#' @export
get_level_geometries.geodimension <- function(gd,
                                              level_name = NULL) {
  stopifnot("Missing level name." = !is.null(level_name))
  if (gd$snake_case) {
    level_name <- my_to_snake_case(level_name)
  }
  level_name <-
    validate_names(names(gd$geolevel), level_name, 'level')
  sort(names(gd$geolevel[[level_name]]$geometry))
}



#' Get level names
#'
#' Get the names of levels included in the `geodimension`.
#'
#' @param gd A `geodimension` object.
#'
#' @return A vector of names.
#'
#' @family information gathering functions
#' @seealso \code{\link{geodimension}}, \code{\link{geolevel}}
#'
#' @examples
#'
#' ln <- gd_us |>
#'   get_level_names()
#'
#' @export
get_level_names <- function(gd) {
  UseMethod("get_level_names")
}

#' @rdname get_level_names
#' @export
get_level_names.geodimension <- function(gd) {
  sort(names(gd$geolevel))
}



#' Get level data
#'
#' Get the data table of a given level.
#'
#' It allows selecting whether we want only the data defined locally in the level
#' or also those that it inherits from other higher levels with which it is related.
#'
#' In case of inheriting attributes from other levels, in the table, these can
#' have as a prefix the name of the level.
#'
#' @param gd A `geodimension` object.
#' @param level_name A string.
#' @param inherited A boolean.
#' @param add_prefix A boolean.
#'
#' @return A `tibble` object.
#'
#' @family information gathering functions
#' @seealso \code{\link{geodimension}}, \code{\link{geolevel}}
#'
#' @examples
#'
#' ld <- gd_us |>
#'   get_level_data(level_name = "county",
#'                  inherited = TRUE)
#'
#' @export
get_level_data <- function(gd,
                           level_name,
                           inherited,
                           add_prefix) {
  UseMethod("get_level_data")
}

#' @rdname get_level_data
#' @export
get_level_data.geodimension <- function(gd,
                                        level_name = NULL,
                                        inherited = FALSE,
                                        add_prefix = TRUE) {
  stopifnot("Missing level name." = !is.null(level_name))
  if (gd$snake_case) {
    level_name <- my_to_snake_case(level_name)
  }
  level_name <-
    validate_names(names(gd$geolevel), level_name, 'level')
  data <- gd$geolevel[[level_name]]$data
  if (inherited) {
    res <- names(gd$relation[[level_name]])
    for (l in res) {
      lower_level_attributes <-
        gd$relation[[level_name]][[l]]$lower_fk
      upper_level_key <-
        gd$relation[[level_name]][[l]]$upper_pk
      d <- get_level_data(gd, level_name = l, inherited, add_prefix)
      if (add_prefix) {
        names(d) <- paste0(l, '_', names(d))
        upper_level_key <- paste0(l, '_', upper_level_key)
      }
      data <- data |>
        dplyr::left_join(d, by = stats::setNames(upper_level_key, lower_level_attributes))
    }
  }
  data
}


#' Get level layer
#'
#' Get a geographic layer associated with a level. We can select the geometry
#' and, using boolean parameters, which attributes are included in the layer's
#' table: only the attributes that make up the key and, if applied to a geodimension,
#' inherited attributes to which the prefix of the level where they are defined
#' can be added.
#'
#' @param gd A `geolevel` or `geodimension` object.
#' @param level_name A string.
#' @param geometry A string.
#' @param only_key A boolean.
#' @param inherited A boolean.
#' @param add_prefix A boolean.
#'
#' @return A `sf` object.
#'
#' @family information gathering functions
#' @seealso \code{\link{geodimension}}, \code{\link{geolevel}}
#'
#' @examples
#'
#' file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
#' layer_us_state <- sf::st_read(file, layer = "state", quiet = TRUE)
#'
#' state <-
#'   geolevel(name = "state",
#'            layer = layer_us_state,
#'            key = "GEOID")
#'
#' state_ll <- state |>
#'   get_level_layer("polygon")
#'
#'
#' county_ll <- gd_us |>
#'   get_level_layer(level_name = "county",
#'                   geometry = "polygon",
#'                   inherited = TRUE)
#'
#' @export
get_level_layer <- function(gd,
                            level_name,
                            geometry,
                            only_key,
                            inherited,
                            add_prefix) UseMethod("get_level_layer")

#' @rdname get_level_layer
#'
#' @export
get_level_layer.geodimension <- function(gd,
                                         level_name = NULL,
                                         geometry = NULL,
                                         only_key = FALSE,
                                         inherited = FALSE,
                                         add_prefix = TRUE) {
  stopifnot("Missing level name." = !is.null(level_name))
  if (gd$snake_case) {
    level_name <- my_to_snake_case(level_name)
  }
  level_name <-
    validate_names(names(gd$geolevel), level_name, 'level')
  if (is.null(geometry)) {
    geometry <- names(gd$geolevel[[level_name]]$geometry)[1]
  } else {
    stopifnot("The selected geometry is not defined for the level." = geometry %in% names(gd$geolevel[[level_name]]$geometry))
  }
  layer <- gd$geolevel[[level_name]]$geometry[[geometry]]
  if (!only_key) {
    data <- gd |>
      get_level_data(level_name = level_name, inherited = inherited, add_prefix = add_prefix)
    layer <- data |>
      dplyr::left_join(layer, by = gd$geolevel[[level_name]]$key) |>
      sf::st_as_sf()
  }
  layer
}



#' Get level data with latitude and longitude
#'
#' Get the data table of a given level with latitude and longitude.
#'
#' It allows selecting whether we want only the data defined locally in the level
#' or also those that it inherits from other higher levels with which it is related.
#'
#' In case of inheriting attributes from other levels, in the table, these can
#' have as a prefix the name of the level.
#'
#' Additionally, we indicate the names of the fields where longitude and latitude
#' will be stored, as well as the crs that is used, if they are different from the
#' default values.
#'
#' @param gd A `geodimension` object.
#' @param level_name A string.
#' @param inherited A boolean.
#' @param add_prefix A boolean.
#' @param lon_lat A vector, name of longitude and latitude attributes.
#' @param crs A coordinate reference system: integer with the EPSG code, or
#'   character with proj4string.
#'
#' @return A `tibble` object.
#'
#' @family information gathering functions
#' @seealso \code{\link{geodimension}}, \code{\link{geolevel}}
#'
#' @examples
#'
#' ld <- gd_us |>
#'   get_level_data_geo(level_name = "county",
#'                      inherited = TRUE)
#'
#' @export
get_level_data_geo <- function(gd,
                               level_name,
                               inherited,
                               add_prefix,
                               lon_lat,
                               crs) {
  UseMethod("get_level_data_geo")
}

#' @rdname get_level_data_geo
#' @export
get_level_data_geo.geodimension <- function(gd,
                                            level_name = NULL,
                                            inherited = FALSE,
                                            add_prefix = TRUE,
                                            lon_lat = c("intptlon", "intptlat"),
                                            crs = 4326) {
  lon_lat <- unique(lon_lat)
  stopifnot("Two attributes must be indicated: longitude and latitude." = length(lon_lat) == 2)
  if (gd$snake_case) {
    level_name <- my_to_snake_case(level_name)
    lon_lat <- my_to_snake_case(lon_lat)
  }
  data <- get_level_data(gd, level_name, inherited, add_prefix)
  layer <- gd$geolevel[[level_name]]$geometry$point
  if (is.null(layer)) {
    gd$geolevel[[level_name]] <-
      complete_point_geometry(gd$geolevel[[level_name]])
    layer <- gd$geolevel[[level_name]]$geometry$point
  }
  if (!is.null(layer)) {
    layer <- data |>
      dplyr::left_join(layer, by = gd$geolevel[[level_name]]$key) |>
      sf::st_as_sf() |>
      sf::st_transform(crs)
    data[, lon_lat] <-
      matrix(unlist(sf::st_geometry(layer)),
             ncol = 2,
             byrow = TRUE)
  }
  data
}
