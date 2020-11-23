#' `geolevel` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @importFrom rlang :=
#'
#' @param name A string, level name.
#' @param layer A `sf` object.
#' @param attributes A vector, selected attributes.
#' @param key A vector, attributes that compose the key.
#'
#' @return A `geolevel` object.
#'
#' @keywords internal
new_geolevel <-
  function(name = NULL,
           layer = NULL,
           attributes = NULL,
           key = NULL) {
    geometry <- get_geometry(layer)
    stopifnot(geometry %in% c("polygon", "point", "line"))

    stopifnot(!is.null(name))
    data <- tibble::tibble((sf::st_drop_geometry(layer)))
    attributes <- unique(attributes)
    stopifnot(attributes %in% names(data))
    if (is.null(attributes)) {
      attributes <-  names(data)
    }

    stopifnot(!is.null(key))
    key <- unique(key)
    stopifnot(key %in% names(data))
    attributes <- unique(c(key, attributes))

    data <- data %>%
      dplyr::select(tidyselect::all_of(attributes)) %>%
      dplyr::group_by_at(attributes) %>%
      dplyr::summarize(.groups = "drop")

    data_key <- data %>%
      dplyr::select(tidyselect::all_of(key)) %>%
      dplyr::group_by_at(key) %>%
      dplyr::summarize(.groups = "drop")

    is_a_valid_key <- (nrow(data) == nrow(data_key))
    stopifnot(is_a_valid_key)

    surrogate_key <- sprintf("%s_key", name)
    data_key <- data_key %>%
      tibble::add_column(!!surrogate_key := 1:nrow(data_key), .before = 1)

    data <- data_key %>%
      dplyr::left_join(data, by = key)

    layer <- layer %>%
      dplyr::select(tidyselect::all_of(key)) %>%
      dplyr::group_by_at(key) %>%
      dplyr::summarize(.groups = "drop")

    # only surrogate key and geometry
    layer <- data_key %>%
      dplyr::left_join(layer, by = key) %>%
      sf::st_as_sf() %>%
      dplyr::select(tidyselect::all_of(names(data_key)[1]))

    # only instances with geometry
    layer <- layer[!is.na(sf::st_dimension(layer)), ]

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
#' @family level definition functions
#' @seealso
#'
#' @examples
#' library(sf)
#'
#' us_region <-
#'   get_level_layer(gd_us_city, level_name = "region", attributes = TRUE)
#'
#' region <-
#'   geolevel(name = "region",
#'            layer = us_region,
#'            key = c("geoid"))
#'
#' @export
geolevel <-
  function(name = NULL,
           layer = NULL,
           attributes = NULL,
           key = NULL) {
    new_geolevel(name, layer, attributes, key)
  }


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
#' us_region <-
#'   get_level_layer(gd_us_city, level_name = "region", attributes = TRUE)
#'
#' geometry <- get_geometry(us_region)
#' # [1] "polygon"
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
  return("other")
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
#' us_region <-
#'   get_level_layer(gd_us_city, level_name = "region", attributes = TRUE)
#'
#' is_key <- check_key(us_region, key = c("name"))
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

