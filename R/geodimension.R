
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
  gd$geolevel[[level$name]] <- level
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
#' gd_1 <- gd |>
#'   relate_levels(lower_level_name = "division",
#'                 upper_level_name = "region",
#'                 by_geography = TRUE)
#'
#' gd_2 <- gd |>
#'   relate_levels(lower_level_name = "division",
#'                 lower_level_attributes = "REGION",
#'                 upper_level_name = "region")
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


#' Check related levels in a dimension
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
#' t <- gd |>
#'   get_unrelated_instances(lower_level_name = "division",
#'                           upper_level_name = "region")
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
#' @family level association functions
#'
#' @examples
#'
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
#'                 lower_level_attributes = "REGION",
#'                 upper_level_name = "region")
#'
#' gd <- gd |>
#'   complete_relation_by_geography(lower_level_name = "division",
#'                                  upper_level_name = "region")
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



#' Get higher level names
#'
#' Get the names of levels included in the `geodimension` that are related to the
#' given level and are upper levels. We can get only the direct levels or the
#' levels reached by passing through other levels.
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
#'
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
#'                 lower_level_attributes = "REGION",
#'                 upper_level_name = "region")
#'
#' ln <- gd |>
#'   get_higher_level_names(level_name = "division",
#'                          indirect_levels = TRUE)
#'
#' @export
get_higher_level_names <- function(gd,
                                   level_name,
                                   indirect_levels) {
  UseMethod("get_higher_level_names")
}


#' @rdname get_higher_level_names
#' @export
get_higher_level_names.geodimension <- function(gd,
                                                level_name = NULL,
                                                indirect_levels = FALSE) {
  stopifnot("Missing level name." = !is.null(level_name))
  level_name <-
    validate_names(names(gd$geolevel), level_name, 'level')
  res <- names(gd$relation[[level_name]])
  if (indirect_levels) {
    for (l in res) {
      r <- get_higher_level_names(gd, level_name = l, indirect_levels)
      res <- union(res, r)
    }
  }
  res
}



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
#'
#' region <-
#'   geolevel(name = "region",
#'            layer = layer_us_region,
#'            key = "geoid")
#'
#' gd <-
#'   geodimension(name = "gd_us",
#'                level = region)
#'
#' lg <- gd |>
#'   get_level_geometries(level_name = "region")
#'
#' @export
get_level_geometries <- function(gd,
                                 level_name) {
  UseMethod("get_level_geometries")
}


#' @rdname get_level_geometries
#' @export
get_level_geometries.geodimension <- function(gd,
                                              level_name = NULL) {
  stopifnot("Missing level name." = !is.null(level_name))
  level_name <-
    validate_names(names(gd$geolevel), level_name, 'level')
  sort(names(gd$geolevel[[level_name]]$geometry))
}



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
#'
#' region <-
#'   geolevel(name = "region",
#'            layer = layer_us_region,
#'            key = "geoid")
#'
#' gd <-
#'   geodimension(name = "gd_us",
#'                level = region)
#'
#' ln <- gd |>
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
#'
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
#'                 lower_level_attributes = "REGION",
#'                 upper_level_name = "region")
#'
#' gd_1 <- gd |>
#'   select_levels(level_names = "region")
#'
#' gd_2 <- gd |>
#'   select_levels(level_names = "division")
#'
#' @export
select_levels <- function(gd, level_names = NULL) {
  UseMethod("select_levels")
}

#' @rdname select_levels
#' @export
select_levels.geodimension <- function(gd, level_names = NULL) {

  stopifnot("Missing level names." = !is.null(level_names))
  level_names <-
    validate_names(names(gd$geolevel), level_names, 'level')
  delete <- setdiff(names(gd$geolevel), level_names)
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
  gd
}


#' Get level data
#'
#' Get the data table of a given level.
#'
#' It allows selecting whether we want only the data defined locally in the level
#' or also those that it inherits from other higher levels with which it is related.
#'
#' In case of inheriting attributes from other levels, in the table, these can
#' have as a prefix the name of the level.
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
#'
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
#'                 lower_level_attributes = "REGION",
#'                 upper_level_name = "region")
#'
#' ld <- gd |>
#'   get_level_data(level_name = "division",
#'                  inherited = TRUE)
#'
#' @export
get_level_data <- function(gd,
                           level_name,
                           inherited,
                           add_prefix) {
  UseMethod("get_level_data")
}


#' @rdname get_level_data
#' @export
get_level_data.geodimension <- function(gd,
                                        level_name = NULL,
                                        inherited = FALSE,
                                        add_prefix = TRUE) {
  stopifnot("Missing level name." = !is.null(level_name))
  level_name <-
    validate_names(names(gd$geolevel), level_name, 'level')
  data <- gd$geolevel[[level_name]]$data
  if (inherited) {
    res <- names(gd$relation[[level_name]])
    for (l in res) {
      lower_level_attributes <-
        gd$relation[[level_name]][[l]]$lower_fk
      upper_level_key <-
        gd$relation[[level_name]][[l]]$upper_pk
      d <- get_level_data(gd, level_name = l, inherited, add_prefix)
      if (add_prefix) {
        names(d) <- paste0(l, '_', names(d))
        upper_level_key <- paste0(l, '_', upper_level_key)
      }
      data <- data |>
        dplyr::left_join(d, by = stats::setNames(upper_level_key, lower_level_attributes))
    }
  }
  data
}


#' Get level layer
#'
#' Get a geographic layer associated with a level. We can select the geometry
#' and, using boolean parameters, which attributes are included in the layer's
#' table: only the attributes that make up the key, the subrogate key, inherited
#' attributes.
#'
#' In case of inheriting attributes from other levels, in the table, these can
#' have as a prefix the name of the level.
#'
#' @param gd A `geodimension` object.
#' @param level_name A string.
#' @param geometry A string.
#' @param only_key A boolean.
#' @param inherited A boolean.
#' @param add_prefix A boolean.
#'
#' @return A `sf` object.
#'
#' @family information output functions
#'
#' @examples
#'
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
#'                 lower_level_attributes = "REGION",
#'                 upper_level_name = "region")
#'
#' ll <- gd |>
#'   get_level_layer(level_name = "division",
#'                   only_key = TRUE)
#'
#' @export
get_level_layer <- function(gd,
                            level_name,
                            geometry,
                            only_key,
                            inherited,
                            add_prefix) {
  UseMethod("get_level_layer")
}


#' @rdname get_level_layer
#' @export
get_level_layer.geodimension <- function(gd,
                                         level_name = NULL,
                                         geometry = NULL,
                                         only_key = FALSE,
                                         inherited = FALSE,
                                         add_prefix = TRUE) {
  stopifnot("Missing level name." = !is.null(level_name))
  level_name <-
    validate_names(names(gd$geolevel), level_name, 'level')
  if (is.null(geometry)) {
    geometry <- names(gd$geolevel[[level_name]]$geometry)[1]
  } else {
    stopifnot("The selected geometry is not defined for the level." = geometry %in% names(gd$geolevel[[level_name]]$geometry))
  }
  layer <- gd$geolevel[[level_name]]$geometry[[geometry]]
  if (!only_key) {
    data <- gd |>
      get_level_data(level_name = level_name, inherited = inherited, add_prefix = add_prefix)
    layer <- data |>
      dplyr::left_join(layer, by = gd$geolevel[[level_name]]$key) |>
      sf::st_as_sf()

  }
  layer
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

