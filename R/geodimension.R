
#' `geodimension` S3 class
#'
#' A `geodimension` object is created. A `geodimension` allows us to relate
#' levels. In addition to the name of the `geodimension` , a `level` has to be
#' given.
#'
#' @param name A string, name of the dimension.
#' @param level A `geolevel`.
#' @param snake_case A boolean, transform all names to snake_case.
#'
#' @return A `geodimension` object.
#'
#' @family geodimension definition functions
#'
#' @examples
#' region <-
#'   geolevel(name = "region",
#'            layer = layer_us_region,
#'            key = "geoid")
#'
#' gd <-
#'   geodimension(name = "gd_us",
#'                level = region,
#'                snake_case = TRUE)
#'
#' @export
geodimension <-
  function(name = NULL,
           level = NULL,
           snake_case = FALSE) {
    stopifnot("Missing geodimension name." = !is.null(name))
    stopifnot("level does not include geolevel object." = methods::is(level, "geolevel"))
    if (snake_case) {
      name <- snakecase::to_snake_case(name)
      level <- snake_case_geolevel(level)
    }
    geolevel <- list()
    geolevel[[level$name]] <- level
    relation <- list()

    geodimension <- list(
      name = name,
      snake_case = snake_case,
      geolevel = geolevel,
      relation = relation
    )

    structure(geodimension,
              class = "geodimension")
  }


#' Add a level to a dimension
#'
#' Once a level is part of the dimension, it can then be related to other levels
#' of the dimension.
#'
#' @param gd A `geodimension` object.
#' @param level A `geolevel`, level to add to the dimension.
#'
#' @return A `geodimension`.
#'
#' @family geodimension definition functions
#'
#' @examples
#' region <-
#'   geolevel(name = "region",
#'            layer = layer_us_region,
#'            key = "geoid")
#'
#' division <-
#'   geolevel(name = "division",
#'            layer = layer_us_division,
#'            key = "geoid")
#'
#' gd <-
#'   geodimension(name = "gd_us",
#'                level = region,
#'                snake_case = TRUE) |>
#'   add_level(division)
#'
#' @export
add_level <- function(gd, level) {
  UseMethod("add_level")
}

#' @rdname add_level
#' @export
add_level.geodimension <- function(gd,
                                   level = NULL) {
  stopifnot("level does not include geolevel object." = methods::is(level, "geolevel"))
  if (gd$snake_case) {
    level <- snake_case_geolevel(level)
  }
  stopifnot("The level was already included in the dimension." = !(level$name %in% names(gd$geolevel)))
  geolevel[[level$name]] <- level
  gd
}


#' Relate levels in a dimension
#'
#' Definition of a direct relationship between two levels of the dimension: the
#' lower level composes the higher level.
#'
#' The relationship may exist by having attributes with common values or by
#' their geographic attributes. In the latter case, the geometry of the upper
#' level must be of the polygon type.
#'
#' To use the geometric relationship, it must be explicitly indicated by the
#' Boolean parameter.
#'
#' If no top-level attributes are indicated, the attributes that make up the key
#' are considered by default, only the corresponding attributes of the lower
#' level have to be indicated.
#'
#' As a special case, if the top level has only one instance, it is not
#' necessary to specify any attributes to define the relationship.
#'
#' @param gd A `geodimension` object.
#' @param lower_level_name A string, name of the lower level.
#' @param lower_level_attributes A vector of attribute names.
#' @param upper_level_name A string, name of the upper lever.
#' @param upper_level_key A vector of attribute names.
#' @param by_geography A boolean.
#'
#' @return A `geodimension`.
#'
#' @family level association functions
#'
#' @examples
#' region <-
#'   geolevel(name = "region",
#'            layer = layer_us_region,
#'            key = "geoid")
#'
#' division <-
#'   geolevel(name = "division",
#'            layer = layer_us_division,
#'            key = "geoid")
#'
#' gd <-
#'   geodimension(name = "gd_us",
#'                level = region) |>
#'   add_level(division)
#'
#' gd <- gd |>
#'   relate_levels(lower_level_name = "division",
#'                 upper_level_name = "region",
#'                 by_geography = TRUE)
#'
#' @export
relate_levels <- function(gd,
                          lower_level_name,
                          lower_level_attributes,
                          upper_level_name,
                          upper_level_key,
                          by_geography) {
  UseMethod("relate_levels")
}


#' @rdname relate_levels
#' @export
relate_levels.geodimension <- function(gd,
                                       lower_level_name = NULL,
                                       lower_level_attributes = NULL,
                                       upper_level_name = NULL,
                                       upper_level_key = NULL,
                                       by_geography = FALSE) {
  stopifnot(lower_level_name %in% names(gd$geolevel))
  stopifnot(upper_level_name %in% names(gd$geolevel))
  lower_level_attributes <- unique(lower_level_attributes)
  if (is.null(upper_level_key)) {
    upper_level_key <- attr(gd$geolevel[[upper_level_name]], "key")
  } else {
    upper_level_key <- unique(upper_level_key)
    upper_level_key_is_a_key <-
      (nrow(gd$geolevel[[upper_level_name]]$data) == nrow(unique(gd$geolevel[[upper_level_name]]$data[, upper_level_key])))
    stopifnot(upper_level_key_is_a_key)
  }
  if (!is.null(lower_level_attributes)) {
    stopifnot(length(lower_level_attributes) == length(upper_level_key))
    stopifnot(lower_level_attributes %in% attr(gd$geolevel[[lower_level_name]], "attributes"))
    stopifnot(upper_level_key %in% attr(gd$geolevel[[upper_level_name]], "attributes"))
  }
  stopifnot(!(upper_level_name %in% names(gd$relation[[lower_level_name]])))

  if (by_geography) {
    stopifnot(is.null(lower_level_attributes))
    stopifnot("polygon" %in% names(gd$geolevel[[upper_level_name]]$geometry))
    lower_geom <- names(gd$geolevel[[lower_level_name]]$geometry)
    if ("point" %in% lower_geom) {
      lower_geom <- "point"
    } else if ("line" %in% lower_geom) {
      lower_geom <- "line"
    }
    layer <- gd$geolevel[[lower_level_name]]$geometry[[lower_geom]]
    if (lower_geom == "polygon") {
      # to avoid warning: make the assumption (that the attribute is constant throughout the geometry)
      sf::st_agr(layer) = "constant"
      layer <- sf::st_point_on_surface(layer)
    }

    res <- sf::st_join(layer, gd$geolevel[[upper_level_name]]$geometry[["polygon"]], join = sf::st_within) |>
      sf::st_drop_geometry()
    names(res) <- c(lower_level_name, upper_level_name)

    multiplicity_n_1 <- nrow(res) == nrow(gd$relation[[lower_level_name]])
    stopifnot(multiplicity_n_1)

    gd$relation[[lower_level_name]] <-
      gd$relation[[lower_level_name]] |>
      dplyr::left_join(res, by = lower_level_name)

  } else if (attr(gd$geolevel[[upper_level_name]], "n_instances_data") == 1) {
    stopifnot(is.null(lower_level_attributes))
    gd$relation[[lower_level_name]] <- gd$relation[[lower_level_name]] |>
      tibble::add_column(!!upper_level_name := gd$relation[[upper_level_name]][[upper_level_name]])
  } else {
    lower_data <-
      gd$geolevel[[lower_level_name]]$data[, c(attr(gd$geolevel[[lower_level_name]], "surrogate_key"),
                                               lower_level_attributes)]
    names(lower_data) <-
      c(attr(gd$geolevel[[lower_level_name]], "surrogate_key"),
        upper_level_key)
    upper_data <-
      gd$geolevel[[upper_level_name]]$data[, c(attr(gd$geolevel[[upper_level_name]], "surrogate_key"),
                                               upper_level_key)]
    lower_data <- lower_data |>
      dplyr::left_join(upper_data, by = upper_level_key) |>
      dplyr::select(c(
        attr(gd$geolevel[[lower_level_name]], "surrogate_key"),
        attr(gd$geolevel[[upper_level_name]], "surrogate_key")
      ))
    names(lower_data) <- c(lower_level_name, upper_level_name)

    multiplicity_n_1 <- nrow(lower_data) == nrow(gd$relation[[lower_level_name]])
    stopifnot(multiplicity_n_1)

    gd$relation[[lower_level_name]] <- gd$relation[[lower_level_name]] |>
      dplyr::left_join(lower_data, by = lower_level_name)

  }
  gd
}
