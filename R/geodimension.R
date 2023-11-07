
#' `geodimension` S3 class
#'
#' A `geodimension` object is created. A `geodimension` allows us to relate
#' levels. In addition to the name of the `geodimension` , a `level` has to be
#' given.
#'
#' @param name A string, name of the dimension.
#' @param level A `geolevel`.
#'
#' @return A `geodimension` object.
#'
#' @family level association functions
#'
#' @examples
#' region <-
#'   geolevel(name = "region",
#'            layer = layer_us_region,
#'            key = "geoid")
#'
#' gd <-
#'   geodimension(name = "gd_us",
#'                level = region)
#'
#' @export
geodimension <- function(name = NULL, level = NULL) {
    geolevel <- list()
    geolevel[[attr(level, "name")]] <- level

    relation <- list()
    data <- level$data[1]
    names(data) <- attr(level, "name")
    relation[[attr(level, "name")]] <- data

    geodimension <- list(geolevel = geolevel, relation = relation)

    structure(
      geodimension,
      name = name,
      class = "geodimension"
    )
  }

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
  stopifnot(level_name %in% names(gd$geolevel))
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
#' @export
add_level <- function(gd,
                      level = NULL) {
  UseMethod("add_level")
}


#' @rdname add_level
#' @export
add_level.geodimension <- function(gd,
                                   level = NULL) {
  stopifnot(!(attr(level, "name") %in% names(gd$geolevel)))
  gd$geolevel[[attr(level, "name")]] <- level

  data <- level$data[1]
  names(data) <- attr(level, "name")
  gd$relation[[attr(level, "name")]] <- data
  gd
}

# get higher level names ---------------------------------------------------------------

#' Get higher level names
#'
#' Get the names of levels included in the `geodimension` that are at a higher
#' level than the indicated level. You can get only the direct levels or the
#' levels reached by passing through other levels.
#'
#' The indicated level may inherit properties of the obtained levels.
#'
#' @param gd A `geodimension` object.
#' @param level_name A string.
#' @param indirect_levels A boolean.
#'
#' @return A vector of names.
#'
#' @family information output functions
#'
#' @examples
#' ln <- gd_us |>
#'   get_higher_level_names(level_name = "state",
#'                          indirect_levels = TRUE)
#'
#' @export
get_higher_level_names <- function(gd,
                                   level_name = NULL,
                                   indirect_levels = FALSE) {
  UseMethod("get_higher_level_names")
}


#' @rdname get_higher_level_names
#' @export
get_higher_level_names.geodimension <- function(gd,
                                                level_name = NULL,
                                                indirect_levels = FALSE) {
  stopifnot(level_name %in% names(gd$geolevel))
  if (indirect_levels) {
    gdil <- calculate_inherited_relationships(gd, level_name = level_name)
    levels <- names(gdil$relation[[level_name]])[-1]
  } else {
    levels <- names(gd$relation[[level_name]])[-1]
  }
  sort(levels)
}

# get level data ---------------------------------------------------------------

#' Get level data
#'
#' Get the data table of a given level.
#'
#' It allows selecting whether we want only the data defined locally in the
#' level or also those that it inherits from other higher levels with which it
#' is related.
#'
#' In case of inheriting attributes from other levels, in the table, these can
#' have as a prefix the name of the level in uppercase.
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
#' ld <- gd_us |>
#'   get_level_data(level_name = "state",
#'                  inherited = TRUE)
#'
#' @export
get_level_data <- function(gd,
                           level_name = NULL,
                           inherited = FALSE,
                           add_prefix = TRUE) {
  UseMethod("get_level_data")
}


#' @rdname get_level_data
#' @export
get_level_data.geodimension <- function(gd,
                                        level_name = NULL,
                                        inherited = FALSE,
                                        add_prefix = TRUE) {
  stopifnot(level_name %in% names(gd$geolevel))
  data <- gd$geolevel[[level_name]]$data
  if (inherited) {
    gd <- calculate_inherited_relationships(gd, level_name = level_name)
    key <- names(data)[1]
    for (rel in sort(names(gd$relation[[level_name]])[-1])) {
      relation <- gd$relation[[level_name]][, c(level_name, rel)]
      if (add_prefix) {
        names(relation)[2] <- paste(toupper(names(relation)[2]), names(relation)[2], sep = "_")
      }
      data <- data |>
        dplyr::left_join(relation, by = stats::setNames(level_name, key))
      rel_data <- gd$geolevel[[rel]]$data
      if (add_prefix) {
        names(rel_data) <- paste(toupper(rel), names(rel_data), sep = "_")
      }
      key_rel <- names(rel_data)[1]
      names(data)[length(names(data))] <- key_rel
      fk <- names(data)[length(names(data))]
      data <- data |>
        dplyr::left_join(rel_data, by = stats::setNames(key_rel, fk))
    }
  }
  data
}

# get level geometries ---------------------------------------------------------------

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
#' lg <- gd_us |>
#'   get_level_geometries(level_name = "state")
#'
#' @export
get_level_geometries <- function(gd,
                                 level_name = NULL) {
  UseMethod("get_level_geometries")
}


#' @rdname get_level_geometries
#' @export
get_level_geometries.geodimension <- function(gd,
                                              level_name = NULL) {
  stopifnot(level_name %in% names(gd$geolevel))
  sort(names(gd$geolevel[[level_name]]$geometry))
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


# get level names ---------------------------------------------------------

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

# unrelated instances -----------------------------------------------------

#' Get unrelated instances
#'
#' Given two levels between which an explicit relationship is defined, it
#' returns the lower-level instances that are not related to any higher-level
#' instances.
#'
#' @param gd A `geodimension` object.
#' @param lower_level_name A string, name of the lower level.
#' @param upper_level_name A string, name of the upper lever.
#'
#' @return A `tibble`.
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
#' ui <- gd |>
#'   get_unrelated_instances(lower_level_name = "division",
#'                           upper_level_name = "region")
#'
#' @export
get_unrelated_instances <- function(gd,
                                    lower_level_name = NULL,
                                    upper_level_name = NULL) {
  UseMethod("get_unrelated_instances")
}


#' @rdname get_unrelated_instances
#' @export
get_unrelated_instances.geodimension <- function(gd,
                                                 lower_level_name = NULL,
                                                 upper_level_name = NULL) {
  stopifnot(lower_level_name %in% names(gd$geolevel))
  stopifnot(upper_level_name %in% names(gd$geolevel))
  stopifnot(upper_level_name %in% names(gd$relation[[lower_level_name]]))

  unrelated <- gd$relation[[lower_level_name]][[lower_level_name]][is.na(gd$relation[[lower_level_name]][[upper_level_name]])]
  gd$geolevel[[lower_level_name]]$data[unrelated, ]
}

# select levels ---------------------------------------------------------

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
#' gds <- gd_us |>
#'   select_levels(level_names = c("division", "region", "nation"))
#'
#' @export
select_levels <- function(gd, level_names = NULL) {
  UseMethod("select_levels")
}


#' @rdname select_levels
#' @export
select_levels.geodimension <- function(gd, level_names = NULL) {
  level_names <- unique(level_names)
  existing_names <- names(gd$geolevel)
  stopifnot(level_names %in% existing_names)
  delete <- generics::setdiff(existing_names, level_names)
  for (del in delete) {
    gd$geolevel[[del]] <- NULL
    gd$relation[[del]] <- NULL
  }
  for (level in names(gd$relation)) {
    rel_names <- generics::setdiff(names(gd$relation[[level]]), delete)
    gd$relation[[level]] <- gd$relation[[level]][, rel_names]
  }
  gd
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
