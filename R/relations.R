#' Relate levels in a dimension
#'
#' Definition of a direct relationship between two levels of the dimension: the
#' lower level composes the higher level.
#'
#' The relationship may exist by having attributes with common values or by
#' their geographic attributes. In the latter case, the geometry of the upper
#' level must be of the polygon type.
#'
#' If no top-level attributes are indicated, the attributes that make up the key
#' are considered by default, only the corresponding attributes of the lower
#' level have to be indicated.
#'
#' To use the geometric relationship, it must be explicitly indicated by the
#' Boolean parameter. In this case, the attributes of the lower level must not
#' exist in the table, they will be added with the values of the key of the upper
#' level, according to the established relationship. If lower level attribute names
#' are not provided, they will be generated from the upper level key names, adding
#' a prefix.
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
#' @family relationship between geolevels
#' @seealso \code{\link{geodimension}}, \code{\link{geolevel}}
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
#' gd <-
#'   geodimension(name = "gd_us",
#'                level = place) |>
#'   add_level(level = county)
#'
#' gd <- gd |>
#'   relate_levels(
#'     lower_level_name = "place",
#'     lower_level_attributes = "county_geoid",
#'     upper_level_name = "county"
#'   )
#'
#' gd_2 <- gd |>
#'   relate_levels(
#'     lower_level_name = "place",
#'     upper_level_name = "county",
#'     by_geography = TRUE
#'   )
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
  stopifnot("Missing lower level name." = !is.null(lower_level_name))
  stopifnot("Missing upper level name." = !is.null(upper_level_name))
  if (gd$snake_case) {
    lower_level_name <- my_to_snake_case(lower_level_name)
    lower_level_attributes <- my_to_snake_case(lower_level_attributes)
    upper_level_name <- my_to_snake_case(upper_level_name)
    upper_level_key <- my_to_snake_case(upper_level_key)
  }
  lower_level_name <-
    validate_names(names(gd$geolevel), lower_level_name, 'lower level')
  upper_level_name <-
    validate_names(names(gd$geolevel), upper_level_name, 'upper level')
  stopifnot(
    "Upper level has more instances than lower level." = nrow(gd$geolevel[[upper_level_name]]$data) <= nrow(gd$geolevel[[lower_level_name]]$data)
  )
  if (!is.null(upper_level_key)) {
    upper_level_key <-
      validate_names(names(gd$geolevel[[upper_level_name]]$data),
                     upper_level_key,
                     'attribute')
    upper_level_key_is_a_key <-
      (nrow(gd$geolevel[[upper_level_name]]$data) == nrow(unique(gd$geolevel[[upper_level_name]]$data[, upper_level_key])))
    stopifnot("upper_level_key is not a valid key." = upper_level_key_is_a_key)
  } else {
    upper_level_key <- gd$geolevel[[upper_level_name]]$key
  }
  if (by_geography) {
    if (is.null(lower_level_attributes)) {
      lower_level_attributes <-
        paste0("fk_", upper_level_name, "_", upper_level_key)
    }
    for (a in lower_level_attributes) {
      stopifnot("The lower level attributes already exist." = !(a %in% names(gd$geolevel[[lower_level_name]]$data)))
    }
  } else {
    stopifnot("Missing lower level attributes." = !is.null(lower_level_attributes))
    lower_level_attributes <-
      validate_names(names(gd$geolevel[[lower_level_name]]$data),
                     lower_level_attributes,
                     'attribute')
  }
  stopifnot(
    "The attributes of the lower level do not correspond to the key of the upper one." = length(lower_level_attributes) == length(upper_level_key)
  )
  hln <-
    get_higher_level_names(gd, level_name = upper_level_name, indirect_levels = TRUE)
  stopifnot(
    "The inverse relationship between the levels is already defined." = !(lower_level_name %in% hln)
  )
  gd$relation[[lower_level_name]] <- list()
  gd$relation[[lower_level_name]][[upper_level_name]] <-
    list(lower_fk = lower_level_attributes,
         upper_pk = upper_level_key)

  if (by_geography) {
    stopifnot(
      "The upper level must include polygon geometry." = "polygon" %in% names(gd$geolevel[[upper_level_name]]$geometry)
    )
    lower_geom <- names(gd$geolevel[[lower_level_name]]$geometry)
    if (!("point" %in% lower_geom)) {
      gd$geolevel[[lower_level_name]] <-
        complete_point_geometry(gd$geolevel[[lower_level_name]])
    }
    layer <- gd$geolevel[[lower_level_name]]$geometry[["point"]]
    res <-
      sf::st_join(layer, gd$geolevel[[upper_level_name]]$geometry[["polygon"]], join = sf::st_within) |>
      sf::st_drop_geometry()
    names(res) <-
      c(gd$geolevel[[lower_level_name]]$key, lower_level_attributes)
    gd$geolevel[[lower_level_name]]$data <-
      gd$geolevel[[lower_level_name]]$data |>
      dplyr::left_join(res, by = gd$geolevel[[lower_level_name]]$key)
  }

  data <- gd$geolevel[[upper_level_name]]$data[, upper_level_key]
  names(data) <- lower_level_attributes
  data <-
    gd$geolevel[[lower_level_name]]$data[, c(gd$geolevel[[lower_level_name]]$key, lower_level_attributes)] |>
    dplyr::inner_join(data, by = lower_level_attributes)
  if (nrow(data) != nrow(gd$geolevel[[lower_level_name]]$data)) {
    warning(
      "There are rows left on the lower level not related to the upper level. Check them using `get_unrelated_instances()`."
    )
  }
  gd
}


#' Get unrelated instances
#'
#' Given two previously related levels of a dimension, it obtains the instances
#' of the lower level that have not been related to the upper level.
#'
#' @param gd A `geodimension` object.
#' @param lower_level_name A string, name of the lower level.
#' @param upper_level_name A string, name of the upper lever.
#'
#' @return A `tibble`, unrelated lower level instances.
#'
#' @family relationship between geolevels
#' @seealso \code{\link{geodimension}}, \code{\link{geolevel}}
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
#' gd <-
#'   geodimension(name = "gd_us",
#'                level = place) |>
#'   add_level(level = county)
#'
#' gd <- gd |>
#'   relate_levels(
#'     lower_level_name = "place",
#'     upper_level_name = "county",
#'     by_geography = TRUE
#'   )
#'
#' ui <- gd |>
#'   get_unrelated_instances(
#'     lower_level_name = "place",
#'     upper_level_name = "county"
#'   )
#'
#' @export
get_unrelated_instances <- function(gd,
                                    lower_level_name,
                                    upper_level_name) {
  UseMethod("get_unrelated_instances")
}

#' @rdname get_unrelated_instances
#' @export
get_unrelated_instances.geodimension <- function(gd,
                                                 lower_level_name = NULL,
                                                 upper_level_name = NULL) {
  stopifnot("Missing lower level name." = !is.null(lower_level_name))
  stopifnot("Missing upper level name." = !is.null(upper_level_name))
  if (gd$snake_case) {
    lower_level_name <- my_to_snake_case(lower_level_name)
    upper_level_name <- my_to_snake_case(upper_level_name)
  }
  lower_level_attributes <-
    gd$relation[[lower_level_name]][[upper_level_name]]$lower_fk
  upper_level_key <-
    gd$relation[[lower_level_name]][[upper_level_name]]$upper_pk
  stopifnot("The levels are not related yet." = !(is.null(lower_level_attributes) |
                                                    is.null(upper_level_key)))
  upper <- gd$geolevel[[upper_level_name]]$data[, upper_level_key]
  names(upper) <- lower_level_attributes
  lower <-
    unique(gd$geolevel[[lower_level_name]]$data[, lower_level_attributes])
  lower <- dplyr::setdiff(lower, upper)
  lower <- gd$geolevel[[lower_level_name]]$data |>
    dplyr::inner_join(lower, by = lower_level_attributes)
  lower
}


#' Complete relation by geography
#'
#' Two levels can be related by attributes or by geography (if the upper level
#' has polygon-type geometry). Once related, if there are unrelated instances,
#' we can try to relate those instances using this function, which considers
#' alternative geographic relationships.
#'
#' It does not necessarily succeed trying to relate instances.
#'
#' @param gd A `geodimension` object.
#' @param lower_level_name A string, name of the lower level.
#' @param upper_level_name A string, name of the upper lever.
#'
#' @return A `geodimension` object.
#'
#' @family relationship between geolevels
#' @seealso \code{\link{geodimension}}, \code{\link{geolevel}}
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
#' gd <-
#'   geodimension(name = "gd_us",
#'                level = place) |>
#'   add_level(level = county)
#'
#' gd <- gd |>
#'   relate_levels(
#'     lower_level_name = "place",
#'     lower_level_attributes = "county_geoid",
#'     upper_level_name = "county"
#'   ) |>
#'   complete_relation_by_geography(
#'     lower_level_name = "place",
#'     upper_level_name = "county"
#'   )
#'
#' @export
complete_relation_by_geography <- function(gd,
                                           lower_level_name = NULL,
                                           upper_level_name = NULL) {
  UseMethod("complete_relation_by_geography")
}

#' @rdname complete_relation_by_geography
#' @export
complete_relation_by_geography.geodimension <- function(gd,
                                                        lower_level_name = NULL,
                                                        upper_level_name = NULL) {
  if (gd$snake_case) {
    lower_level_name <- my_to_snake_case(lower_level_name)
    upper_level_name <- my_to_snake_case(upper_level_name)
  }
  t <-
    get_unrelated_instances.geodimension(gd, lower_level_name, upper_level_name)
  if (nrow(t) > 0) {
    gd$geolevel[[lower_level_name]]$data <-
      gd$geolevel[[lower_level_name]]$data |>
      dplyr::setdiff(t)
    lower_level_attributes <-
      gd$relation[[lower_level_name]][[upper_level_name]]$lower_fk
    upper_level_key <-
      gd$relation[[lower_level_name]][[upper_level_name]]$upper_pk
    stopifnot(
      "The upper level must include polygon geometry." = "polygon" %in% names(gd$geolevel[[upper_level_name]]$geometry)
    )
    lower_geom <- names(gd$geolevel[[lower_level_name]]$geometry)
    if (!("point" %in% lower_geom)) {
      gd$geolevel[[lower_level_name]] <-
        complete_point_geometry(gd$geolevel[[lower_level_name]])
    }
    layer <- gd$geolevel[[lower_level_name]]$geometry[["point"]]
    res <-
      sf::st_join(layer, gd$geolevel[[upper_level_name]]$geometry[["polygon"]], join = sf::st_within) |>
      sf::st_drop_geometry()
    names(res) <-
      c(gd$geolevel[[lower_level_name]]$key, lower_level_attributes)
    att <- names(t)
    t <- t[, setdiff(att, lower_level_attributes)]
    t <- t |>
      dplyr::left_join(res, by = gd$geolevel[[lower_level_name]]$key)
    t <- t[, att]
    gd$geolevel[[lower_level_name]]$data <-
      gd$geolevel[[lower_level_name]]$data |>
      dplyr::union_all(t)
  }
  gd
}


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
#' @family relationship between geolevels
#' @seealso \code{\link{geodimension}}, \code{\link{geolevel}}
#'
#' @examples
#'
#' gd_us_2 <- gd_us |>
#'   select_levels(level_names = c("state", "county", "place", "region"))
#'
#' @export
select_levels <- function(gd, level_names = NULL) {
  UseMethod("select_levels")
}

#' @rdname select_levels
#' @export
select_levels.geodimension <- function(gd, level_names = NULL) {

  stopifnot("Missing level names." = !is.null(level_names))
  if (gd$snake_case) {
    level_names <- my_to_snake_case(level_names)
  }
  level_names <-
    validate_names(names(gd$geolevel), level_names, 'level')
  delete <- setdiff(names(gd$geolevel), level_names)
  gdp <- gd
  for (l in delete) {
    gd$geolevel[[l]] <- NULL
    gd$relation[[l]] <- NULL
  }
  for (r in names(gd$relation)) {
    for (l in delete) {
      gd$relation[[r]][[l]] <- NULL
    }
    if (length(gd$relation[[r]]) == 0) {
      gd$relation[[r]] <- NULL
    }
  }
  # sort by number of instances of the table
  nr <- NULL
  for (l in level_names) {
    nr <- c(nr, nrow(gd$geolevel[[l]]$data))
  }
  names(nr) <- level_names
  nr <- sort(nr)
  # lost relationships
  for (l in names(nr)) {
    hlp <- get_higher_level_names(gdp, level_name = l, indirect_levels = TRUE)
    hlp <- intersect(hlp, level_names)
    hl <- get_higher_level_names(gd, level_name = l, indirect_levels = TRUE)
    lr <- setdiff(hlp, hl)
    if (length(lr) > 0) {
      for (h in lr) {
        gd <- define_relationship(gd, gdp, l, h)
      }
    }
  }
  gd
}



#' define relationship
#'
#' @param gd A `geolevel` object.
#' @param gdp A `geolevel` object.
#' @param l A string, name of the lower level.
#' @param h A string, name of the upper lever.
#'
#' @return A `geolevel` object.
#'
#' @keywords internal
define_relationship <- function(gd, gdp, l, h) {
  data <- get_level_keys(gdp, level_name = l)
  att <- names(data)
  att <- att[startsWith(att, paste0(l, 'XXX')) | startsWith(att, paste0(h, 'XXX'))]
  data <- data[, att]
  lkey <- gdp$geolevel[[l]]$key
  hkey <- paste0("fk_", h, '_', gdp$geolevel[[h]]$key)
  names(data) <- c(lkey, hkey)

  gd$geolevel[[l]]$data <- gd$geolevel[[l]]$data |>
    dplyr::left_join(data, by = lkey)

  gd <- gd |>
    relate_levels(
      lower_level_name = l,
      lower_level_attributes = hkey,
      upper_level_name = h
    )
  gd
}


#' get level keys
#'
#' Starting from a level, the keys of all the levels above it.
#'
#' @param gd A `geolevel` object.
#' @param level_name A string, name of the level.
#'
#' @return A `tibble` object.
#'
#' @keywords internal
get_level_keys <- function(gd, level_name = NULL) {
  att <- gd$geolevel[[level_name]]$key
  ln <- paste0(level_name, 'XXX', att)
  data <- gd$geolevel[[level_name]]$data
  res <- names(gd$relation[[level_name]])
  for (l in res) {
    lower_level_attributes <- gd$relation[[level_name]][[l]]$lower_fk
    upper_level_key <- gd$relation[[level_name]][[l]]$upper_pk
    att <- c(att, lower_level_attributes)
    ln <- c(ln, paste0(l, 'XXX', upper_level_key))
  }
  data <- data[, att]
  names(data) <- ln

  for (l in res) {
    upper_level_key <- gd$relation[[level_name]][[l]]$upper_pk
    upper_level_key <- paste0(l, 'XXX', upper_level_key)

    d <- get_level_keys(gd, level_name = l)
    # in case of redundant relationships
    unique_att <- c(upper_level_key, setdiff(names(d), names(data)))
    d <- d[, unique_att]
    data <- data |>
      dplyr::left_join(d, by = upper_level_key)
  }
  data
}
