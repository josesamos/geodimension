

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



#' @rdname get_level_layer
#'
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

