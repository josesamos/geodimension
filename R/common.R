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
#' @family level definition functions
#' @seealso
#'
#' @examples
#' library(sf)
#'
#' geometry <- get_geometry(layer_us_region)
#'
#' @export
get_geometry <- function(layer) {
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
#' @family level definition functions
#' @seealso
#'
#' @examples
#' library(sf)
#'
#' is_key <- check_key(layer_us_region, key = c("name"))
#'
#' @export
check_key <- function(table, key = NULL) {
  if ("sf" %in% class(table)) {
    table <- tibble::tibble((sf::st_drop_geometry(table)))
  }
  stopifnot(!is.null(key))
  key <- unique(key)
  stopifnot(key %in% names(table))

  table_key <- table %>%
    dplyr::select(tidyselect::all_of(key)) %>%
    dplyr::group_by_at(key) %>%
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
#' The CRS of the new layer is indicated. If a CRS is not indicated, it
#' considers the layer's CRS by default and, if it is not a layer, it considers
#' 4326 CRS (WGS84).
#'
#' @param table A `tibble` object.
#' @param lon_lat A vector, name of longitude and latitude attributes.
#' @param crs A coordinate reference system: integer with the EPSG code, or
#'   character with proj4string.
#'
#' @return A `sf` object.
#'
#' @family level definition functions
#' @seealso
#'
#' @examples
#' library(sf)
#'
#' us_state_point <-
#'   coordinates_to_geometry(layer_us_state,
#'                           lon_lat = c("intptlon", "intptlat"))
#'
#' @export
coordinates_to_geometry <- function(table, lon_lat = NULL, crs = NULL) {
  if ("sf" %in% class(table)) {
    if (is.null(crs)) {
      crs <- sf::st_crs(table)
    }
    table <- tibble::tibble((sf::st_drop_geometry(table)))
  }
  stopifnot(!is.null(lon_lat))
  lon_lat <- unique(lon_lat)
  stopifnot(length(lon_lat) == 2)
  stopifnot(lon_lat %in% names(table))
  if (is.null(crs)) {
    crs <- 4326 # WGS84
  }

  table %>%
    sf::st_as_sf(
      coords = lon_lat,
      crs = crs,
      remove = TRUE
    )
}

