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
#' of the layer to be included in the level are indicated, and the subset of
#' these that make up the natural key. If no attribute is indicated, all are
#' considered.
#'
#' A level can have several associated geometries (point, polygon or line), the
#' geometry of the layer is indicated.
#'
#' The *Top* level of any dimension is a special case since it has only one
#' object. This situation is indicated by a boolean parameter.
#'
#' @param name A string, level name.
#' @param layer A `sf` object.
#' @param attributes A vector, selected attributes.
#' @param key A vector, attributes that compose the key.
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
geolevel <-
  function(name = NULL,
           layer = NULL,
           attributes = NULL,
           key = NULL) {
    new_geolevel(name, layer, attributes, key)
  }


# -----------------------------------------------------------------------

#' get_geometry
#'
#'
#' @param layer A `sf` object.
#'
#' @return A string.
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

