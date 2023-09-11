
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
#' region <-
#'   geolevel(name = "region",
#'            layer = layer_us_region,
#'            key = c("geoid"))
#'
#' @export
geolevel <-
  function(name = NULL,
           layer = NULL,
           attributes = NULL,
           key = NULL) {

    browser()

    stopifnot("Missing geolevel name." = !is.null(name))
    stopifnot("layer does not include sf object." = methods::is(layer, "sf"))
    geometry <- get_geometry(layer)
    if (!(geometry %in% c("polygon", "point", "line"))) {
      stop(sprintf('layer has unsupported geometry: %s.', geometry[1]))
    }

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

    data <- data |>
      dplyr::select(tidyselect::all_of(attributes)) |>
      dplyr::group_by_at(attributes) |>
      dplyr::summarize(.groups = "drop")

    data_key <- data |>
      dplyr::select(tidyselect::all_of(key)) |>
      dplyr::group_by_at(key) |>
      dplyr::summarize(.groups = "drop")

    is_a_valid_key <- (nrow(data) == nrow(data_key))
    stopifnot(is_a_valid_key)

    surrogate_key <- sprintf("%s_key", name)
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
