# -----------------------------------------------------------------------

#' Get geometry
#'
#' Get the geometry of a layer, as it is interpreted to define a `geolevel`
#' object.
#'
#' It will only be valid if one of the three geometries is interpreted: *point*,
#' *line* or *polygon*.
#'
#' @param layer A `sf` object.
#'
#' @return A string.
#'
#' @family geolevel definition functions
#'
#' @examples
#'
#' file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
#' layer_us_county <- sf::st_read(file, layer = "county", quiet = TRUE)
#'
#' geometry <- get_geometry(layer_us_county)
#'
#' @export
get_geometry <- function(layer) {
  layer <- sf::st_as_sf(layer)
  geo <- unique(as.character(sf::st_geometry_type(layer, by_geometry = TRUE)))
  if (length(intersect(geo, c("CIRCULARSTRING", "CURVEPOLYGON", "MULTIPOLYGON", "TRIANGLE", "POLYGON"))) > 0) {
    return("polygon")
  } else if (length(intersect(geo, c("LINESTRING", "MULTILINESTRING", "CURVE", "MULTICURVE", "COMPOUNDCURVE"))) > 0) {
    return("line")
  } else if (length(intersect(geo, c("POINT", "MULTIPOINT"))) > 0) {
    return("point")
  }
  geo
}


# -----------------------------------------------------------------------

#' Check key
#'
#' Check if the specified set of attributes can be the key of the table.
#'
#' The table can be a data table or a vector layer.
#'
#' @param table A `tibble` object.
#' @param key A vector, attributes that compose the key.
#'
#' @return A boolean.
#'
#' @family geolevel definition functions
#'
#' @examples
#'
#' file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
#' layer_us_county <- sf::st_read(file, layer = "county", quiet = TRUE)
#'
#' is_key <- check_key(layer_us_county, key = c("STATEFP", "NAME"))
#'
#' @export
check_key <- function(table, key = NULL) {
  if ("sf" %in% class(table)) {
    table <- tibble::tibble((sf::st_drop_geometry(table)))
  }
  stopifnot("The attributes that make up the key need to be indicated." = !is.null(key))
  key <- validate_names(names(table), key, 'attribute')

  table_key <- table |>
    dplyr::select(tidyselect::all_of(key)) |>
    dplyr::group_by_at(key) |>
    dplyr::summarize(.groups = "drop")

  (nrow(table) == nrow(table_key))
}


# -----------------------------------------------------------------------

#' Transform coordinates to point geometry
#'
#' From the coordinates defined in fields such as latitude and longitude, it
#' returns a layer of points.
#'
#' If we start from a geographic layer, it initially transforms it into a table.
#'
#' The CRS of the new layer is indicated. By default, it considers 4326 (WGS84).
#'
#' @param table A `tibble` object.
#' @param lon_lat A vector, name of longitude and latitude attributes.
#' @param crs A coordinate reference system: integer with the EPSG code, or
#'   character with proj4string.
#'
#' @return A `sf` object.
#'
#' @family geolevel definition functions
#'
#' @examples
#'
#' file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
#' layer_us_county <- sf::st_read(file, layer = "county", quiet = TRUE)
#'
#' us_county_point <-
#'   coordinates_to_geometry(layer_us_county,
#'                           lon_lat = c("INTPTLON", "INTPTLAT"))
#'
#' @export
coordinates_to_geometry <- function(table, lon_lat = c("intptlon", "intptlat"), crs = 4326) {
  if ("sf" %in% class(table)) {
    table <- tibble::tibble((sf::st_drop_geometry(table)))
  }
  lon_lat <- unique(lon_lat)
  stopifnot("Two attributes must be indicated: longitude and latitude." = length(lon_lat) == 2)
  names <- names(table)
  lon <- grep(lon_lat[1], names, ignore.case = TRUE)
  lat <- grep(lon_lat[2], names, ignore.case = TRUE)
  stopifnot("Two attributes of the table must be indicated." = length(lon) > 0 & length(lat) > 0)
  if (is.null(crs)) {
    crs <- 4326 # WGS84
  }

  table |>
    sf::st_as_sf(
      coords = names[c(lon, lat)],
      crs = crs,
      remove = TRUE
    )
}

# -----------------------------------------------------------------------

#' Validate names
#'
#' @param defined_names A vector of strings, defined attribute names.
#' @param names A vector of strings, new attribute names.
#' @param concept A string, treated concept.
#' @param repeated A boolean, repeated names allowed.
#'
#' @return A vector of strings, names.
#'
#' @keywords internal
validate_names <- function(defined_names, names, concept = 'name', repeated = FALSE) {
  if (length(names) == 0) {
    names <- defined_names
  } else {
    if (!repeated) {
      stopifnot("There are repeated values." = length(names) == length(unique(names)))
    }
    for (name in names) {
      if (!(name %in% defined_names)) {
        stop(sprintf(
          "'%s' is not defined as %s.",
          name, concept
        ))
      }
    }
  }
  names
}


#' To snake case
#'
#' @param str A string.
#'
#' @return A vector of strings.
#'
#' @keywords internal
my_to_snake_case <- function(str) {
  if (!is.null(str)) {
    str <- snakecase::to_snake_case(str)
  }
  str
}


#' Add prefix
#'
#' @param str A string.
#' @param prefix A string.
#'
#' @return A string.
#'
#' @keywords internal
add_prefix <- function(str, prefix) {
  if (!is.null(str)) {
    str <- paste0(prefix, '_', str)
    str <- gsub(paste0(prefix, '_', prefix, '_'), paste0(prefix, '_'), str)
    str <- gsub(paste0(prefix, '_', prefix), prefix, str)
  }
  str
}



#' All attributes are character
#'
#' @param instances A tibble.
#'
#' @return A tibble.
#'
#' @keywords internal
all_attributes_character <- function(instances) {
  n_row <- nrow(instances)
  attributes <- names(instances)
  # all attributes of type character
  instances[, attributes] <- data.frame(lapply(instances[, attributes], as.character), stringsAsFactors = FALSE)

  if (n_row == 1) {
    instances <- tibble::as_tibble_row(instances)
  } else {
    instances <- tibble::as_tibble(instances)
  }
  instances
}


