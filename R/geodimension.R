
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
  stopifnot("Upper level has more instances than lower level." = nrow(gd$geolevel[[upper_level_name]]$data) <= nrow(gd$geolevel[[lower_level_name]]$data))
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
      lower_level_attributes <- paste0("fk_", upper_level_name, "_", upper_level_key)
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

  gd$relation[[lower_level_name]] <- list()
  gd$relation[[lower_level_name]][[upper_level_name]] <-
    list(lower_fk = lower_level_attributes,
         upper_pk = upper_level_key)

  if (by_geography) {
    stopifnot("The upper level must include polygon geometry." = "polygon" %in% names(gd$geolevel[[upper_level_name]]$geometry))
    lower_geom <- names(gd$geolevel[[lower_level_name]]$geometry)
    if (!("point" %in% lower_geom)) {
      gd$geolevel[[lower_level_name]] <- complete_point_geometry(gd$geolevel[[lower_level_name]])
    }
    layer <- gd$geolevel[[lower_level_name]]$geometry[["point"]]
    res <- sf::st_join(layer, gd$geolevel[[upper_level_name]]$geometry[["polygon"]], join = sf::st_within) |>
      sf::st_drop_geometry()
    names(res) <- c(gd$geolevel[[lower_level_name]]$key, lower_level_attributes)
    gd$geolevel[[lower_level_name]]$data <- gd$geolevel[[lower_level_name]]$data |>
      dplyr::left_join(res, by = gd$geolevel[[lower_level_name]]$key)
  }

  data <- gd$geolevel[[upper_level_name]]$data[, upper_level_key]
  names(data) <- lower_level_attributes
  data <- gd$geolevel[[lower_level_name]]$data[, c(gd$geolevel[[lower_level_name]]$key, lower_level_attributes)] |>
    dplyr::inner_join(data, by = lower_level_attributes)
  if (nrow(data) != nrow(gd$geolevel[[lower_level_name]]$data)) {
    warning(
      "There are rows left on the lower level not related to the upper level. Check them using `check_related_levels()`."
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
#'   check_related_levels(lower_level_name = "division",
#'                        upper_level_name = "region")
#'
#' @export
check_related_levels <- function(gd,
                          lower_level_name,
                          upper_level_name) {
  UseMethod("check_related_levels")
}


#' @rdname check_related_levels
#' @export
check_related_levels.geodimension <- function(gd,
                                       lower_level_name = NULL,
                                       upper_level_name = NULL) {
  stopifnot("Missing lower level name." = !is.null(lower_level_name))
  stopifnot("Missing upper level name." = !is.null(upper_level_name))
  lower_level_attributes <- gd$relation[[lower_level_name]][[upper_level_name]]$lower_fk
  upper_level_key <- gd$relation[[lower_level_name]][[upper_level_name]]$upper_pk
  stopifnot("The levels are not related yet." = !(is.null(lower_level_attributes) | is.null(upper_level_key)))
  upper <- gd$geolevel[[upper_level_name]]$data[, upper_level_key]
  names(upper) <- lower_level_attributes
  lower <- unique(gd$geolevel[[lower_level_name]]$data[, lower_level_attributes])
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
  t <- check_related_levels.geodimension(gd, lower_level_name, upper_level_name)
  if (nrow(t) > 0) {
    gd$geolevel[[lower_level_name]]$data <- gd$geolevel[[lower_level_name]]$data |>
      dplyr::setdiff(t)
    lower_level_attributes <- gd$relation[[lower_level_name]][[upper_level_name]]$lower_fk
    upper_level_key <- gd$relation[[lower_level_name]][[upper_level_name]]$upper_pk
    stopifnot("The upper level must include polygon geometry." = "polygon" %in% names(gd$geolevel[[upper_level_name]]$geometry))
    lower_geom <- names(gd$geolevel[[lower_level_name]]$geometry)
    if (!("point" %in% lower_geom)) {
      gd$geolevel[[lower_level_name]] <- complete_point_geometry(gd$geolevel[[lower_level_name]])
    }
    layer <- gd$geolevel[[lower_level_name]]$geometry[["point"]]
    res <- sf::st_join(layer, gd$geolevel[[upper_level_name]]$geometry[["polygon"]], join = sf::st_within) |>
      sf::st_drop_geometry()
    names(res) <- c(gd$geolevel[[lower_level_name]]$key, lower_level_attributes)
    att <- names(t)
    t <- t[, setdiff(att, lower_level_attributes)]
    t <- t |>
      dplyr::left_join(res, by = gd$geolevel[[lower_level_name]]$key)
    t <- t[, att]
    gd$geolevel[[lower_level_name]]$data <- gd$geolevel[[lower_level_name]]$data |>
      dplyr::union_all(t)
  }
  gd
}

