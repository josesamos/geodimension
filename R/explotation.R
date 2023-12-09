

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
#' @family information output functions
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


#' Select levels
#'
#' Select a subset of the levels of the dimension so that the rest of the levels
#' no longer belong to it.
#'
#' @param gd A `geodimension` object.
#' @param level_names A vector of names.
#'
#' @return A `geodimension` object.
#'
#' @family configuration functions
#'
#' @examples
#'
#' gd_us_2 <- gd_us |>
#'   select_levels(level_names = c("state", "county", "place", "region"))
#'
#' @export
select_levels <- function(gd, level_names = NULL) {
  UseMethod("select_levels")
}

#' @rdname select_levels
#' @export
select_levels.geodimension <- function(gd, level_names = NULL) {

  stopifnot("Missing level names." = !is.null(level_names))
  if (gd$snake_case) {
    level_names <- my_to_snake_case(level_names)
  }
  level_names <-
    validate_names(names(gd$geolevel), level_names, 'level')
  delete <- setdiff(names(gd$geolevel), level_names)
  for (l in delete) {
    gd$geolevel[[l]] <- NULL
    gd$relation[[l]] <- NULL
  }
  for (r in names(gd$relation)) {
    for (l in delete) {
      gd$relation[[r]][[l]] <- NULL
    }
    if (length(gd$relation[[r]]) == 0) {
      gd$relation[[r]] <- NULL
    }
  }
  gd
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
#' @family information output functions
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



#' Set level data
#'
#' Set the data table of a given level.
#'
#' We can get the table, filter or transform the data and redefine the level table.
#'
#' It is checked that the attributes that have been used in the relationships
#' remain in the table.
#'
#' @param gd A `geodimension` object.
#' @param level_name A string.
#' @param data A `tibble` object.
#'
#' @return A `geodimension` object.
#'
#' @family information output functions
#'
#' @examples
#'
#' ld <- gd_us |>
#'   get_level_data(level_name = "county",
#'                  inherited = TRUE)
#'
#' gd_us <- gd_us |>
#'   set_level_data(level_name = "county",
#'                  data = ld)
#'
#' @export
set_level_data <- function(gd,
                           level_name,
                           data) {
  UseMethod("set_level_data")
}

#' @rdname set_level_data
#' @export
set_level_data.geodimension <- function(gd,
                                        level_name = NULL,
                                        data = NULL) {
  stopifnot("Missing level name." = !is.null(level_name))
  if (gd$snake_case) {
    level_name <- my_to_snake_case(level_name)
    names(data) <- my_to_snake_case(names(data))
  }
  level_name <- validate_names(names(gd$geolevel), level_name, 'level')
  gd$geolevel[[level_name]]$data <- data

  attributes <- names(data)
  validate_names(attributes, gd$geolevel[[level_name]]$key, 'key')
  for (l in names(gd$relation[[level_name]])) {
    validate_names(attributes, gd$relation[[level_name]][[l]]$lower_fk, 'attribute')
  }
  for (l in names(gd$relation)) {
    validate_names(attributes, gd$relation[[l]][[level_name]]$upper_pk, 'attribute')
  }
  gd
}



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
#' @family information output functions
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
