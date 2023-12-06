





# calculate inherited relationships ---------------------------------------

#' calculate inherited relationships
#'
#' Each level has explicitly defined relationships with other levels. For a
#' given level, all the relationships with the levels of the dimension, direct
#' and indirect, are obtained.
#'
#' @param gd A `geodimension` object.
#' @param level_name A string, name of the lower level.
#'
#' @keywords internal
calculate_inherited_relationships <- function(gd,
                                              level_name = NULL) {
  stopifnot("Missing level name." = !is.null(level_name))
  stopifnot(level_name %in% names(gd$relation))

  upper_level_names <- names(gd$relation[[level_name]])[-1]
  names_new <- upper_level_names
  already_considered <- NULL
  while (length(names_new) > 0) {
    already_considered <- c(already_considered, names_new)
    for (upper_level in names_new) {
      rel_names <- names(gd$relation[[upper_level]])[-1]
      rel_names <- generics::setdiff(rel_names, upper_level_names)
      for (rel in rel_names) {
        gd$relation[[level_name]] <- gd$relation[[level_name]] |>
          dplyr::left_join(gd$relation[[upper_level]][, c(upper_level, rel)], by = upper_level)
      }
      upper_level_names <- names(gd$relation[[level_name]])[-1]
    }
    names_new <- generics::setdiff(upper_level_names, already_considered)
  }
  gd
}





# get level layer ---------------------------------------------------------------

#' Get level layer
#'
#' Get a geographic layer associated with a level. We can select the geometry
#' and, using boolean parameters, which attributes are included in the layer's
#' table: only the attributes that make up the key, the subrogate key, inherited
#' attributes.
#'
#' In case of inheriting attributes from other levels, in the table, these can
#' have as a prefix the name of the level in uppercase.
#'
#' @param gd A `geodimension` object.
#' @param level_name A string.
#' @param geometry A string.
#' @param only_key A boolean.
#' @param surrogate_key A boolean.
#' @param inherited A boolean.
#' @param add_prefix A boolean.
#'
#' @return A `sf` object.
#'
#' @family information output functions
#'
#' @examples
#' ll <- gd_us |>
#'   get_level_layer(level_name = "division",
#'                   only_key = TRUE,
#'                   surrogate_key = TRUE)
#'
#' @export
get_level_layer <- function(gd,
                            level_name = NULL,
                            geometry = NULL,
                            only_key = FALSE,
                            surrogate_key = FALSE,
                            inherited = FALSE,
                            add_prefix = TRUE) {
  UseMethod("get_level_layer")
}


#' @rdname get_level_layer
#' @export
get_level_layer.geodimension <- function(gd,
                                         level_name = NULL,
                                         geometry = NULL,
                                         only_key = FALSE,
                                         surrogate_key = FALSE,
                                         inherited = FALSE,
                                         add_prefix = TRUE) {
  stopifnot(level_name %in% names(gd$geolevel))
  if (is.null(geometry)) {
    geometry <- names(gd$geolevel[[level_name]]$geometry)[1]
  } else {
    stopifnot(geometry %in% names(gd$geolevel[[level_name]]$geometry))
  }
  layer <- gd$geolevel[[level_name]]$geometry[[geometry]]
  data <- gd |>
    get_level_data(level_name = level_name, inherited = inherited, add_prefix = add_prefix)
  if (only_key) {
    data <- data |>
      dplyr::select(c(attr(gd$geolevel[[level_name]], "surrogate_key"), attr(gd$geolevel[[level_name]], "key")))
  }
  if (surrogate_key) {
    sel <- NULL
  } else {
    sel <- c(1)
  }

  data |>
    dplyr::left_join(layer, by = names(data)[1]) |>
    dplyr::select(!sel) |>
    sf::st_as_sf()
}



# Transform crs ---------------------------------------------------------

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
#' @export
transform_crs <- function(gd,
                          crs = NULL) {
  UseMethod("transform_crs")
}

#' @rdname transform_crs
#' @export
transform_crs.geodimension <- function(gd,
                                       crs = NULL) {
  stopifnot(!is.null(crs))
  for (layer in names(gd$geolevel)) {
    for (geom in names(gd$geolevel[[layer]]$geometry)) {
      gd$geolevel[[layer]]$geometry[[geom]] <-
        gd$geolevel[[layer]]$geometry[[geom]] |>
        sf::st_transform(crs = crs, use_gdal = FALSE)
    }
  }
  gd
}

