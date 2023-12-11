
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
#' @seealso \code{\link{geolevel}}, \code{\link{relate_levels}}, \code{\link{get_level_layer}}
#'
#' @examples
#'
#' file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
#' layer_us_place <- sf::st_read(file, layer = "place", quiet = TRUE)
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
      name <- my_to_snake_case(name)
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
#' @seealso \code{\link{geolevel}}, \code{\link{relate_levels}}, \code{\link{get_level_layer}}
#'
#' @examples
#'
#' file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
#' layer_us_place <- sf::st_read(file, layer = "place", quiet = TRUE)
#' layer_us_county <- sf::st_read(file, layer = "county", quiet = TRUE)
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
#' @family geodimension definition functions
#' @seealso \code{\link{geolevel}}, \code{\link{get_level_data}}
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

  to_validate <- gd$geolevel[[level_name]]$key
  for (l in names(gd$relation[[level_name]])) {
    to_validate <- c(to_validate, gd$relation[[level_name]][[l]]$lower_fk)
  }
  for (l in names(gd$relation)) {
    to_validate <- c(to_validate, gd$relation[[l]][[level_name]]$upper_pk)
  }
  to_validate <- unique(to_validate)
  to_validate_prefix <- add_prefix(to_validate, level_name)
  with_prefix <- TRUE
  for (v in to_validate_prefix) {
    if (!(v %in% attributes)) {
      with_prefix <- FALSE
    }
  }
  if (with_prefix) {
    gd$geolevel[[level_name]]$key <-
      add_prefix(gd$geolevel[[level_name]]$key, level_name)
    for (l in names(gd$relation[[level_name]])) {
      gd$relation[[level_name]][[l]]$lower_fk <-
        add_prefix(gd$relation[[level_name]][[l]]$lower_fk, level_name)
    }
    for (l in names(gd$relation)) {
      gd$relation[[l]][[level_name]]$upper_pk <-
        add_prefix(gd$relation[[l]][[level_name]]$upper_pk, level_name)
    }
    to_validate <- to_validate_prefix
  }
  validate_names(attributes, to_validate, 'attribute')
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
#' @family geodimension definition functions
#' @seealso \code{\link{geolevel}}, \code{\link{relate_levels}}, \code{\link{get_level_layer}}
#'
#' @examples
#'
#' file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
#' layer_us_place <- sf::st_read(file, layer = "place", quiet = TRUE)
#' layer_us_county <- sf::st_read(file, layer = "county", quiet = TRUE)
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
#'   add_level(level = county) |>
#'   transform_crs(crs = 3857)
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

