
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
#'
#' place <-
#'   geolevel(name = "place",
#'            layer = layer_us_place,
#'            key = "GEOID")
#' gd <-
#'   geodimension(name = "gd_us",
#'                level = place)
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
#'
#' place <-
#'   geolevel(name = "place",
#'            layer = layer_us_place,
#'            attributes = c("STATEFP", "county_geoid", "NAME", "type"),
#'            key = "GEOID")
#'
#' county <-
#'   geolevel(
#'     name = "county",
#'     layer = layer_us_county,
#'     attributes = c("STATEFP", "NAME", "type"),
#'     key = "GEOID"
#'   ) |>
#'   add_geometry(coordinates_to_geometry(layer_us_county,
#'                                        lon_lat = c("INTPTLON", "INTPTLAT")))
#'
#' gd_us <-
#'   geodimension(name = "gd_us",
#'                level = place) |>
#'   add_level(level = county)
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
  gd$geolevel[[level$name]] <- level
  gd
}



#' Transform CRS
#'
#' Transform the CRS of all the layers included in the dimension to the one
#' indicated.
#'
#' @param gd A `geodimension` object.
#' @param crs A coordinate reference system: integer with the EPSG code, or
#'   character with proj4string.
#'
#' @return A `geodimension`.
#'
#' @family configuration functions
#'
#' @examples
#'
#' @export
transform_crs <- function(gd,
                          crs = NULL) {
  UseMethod("transform_crs")
}

#' @rdname transform_crs
#' @export
transform_crs.geodimension <- function(gd,
                                       crs = NULL) {
  stopifnot("The crs is missing." = !is.null(crs))
  for (layer in names(gd$geolevel)) {
    for (geom in names(gd$geolevel[[layer]]$geometry)) {
      gd$geolevel[[layer]]$geometry[[geom]] <-
        gd$geolevel[[layer]]$geometry[[geom]] |>
        sf::st_transform(crs = crs, use_gdal = FALSE)
    }
  }
  gd
}

