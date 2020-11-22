

# get level names ---------------------------------------------------------

#' Add a level to a dimension
#'
#' Add a level to a dimension. This level can then be related to other levels of
#' the dimension.
#'
#' @param gd A `geodimension` object.
#'
#' @return A vector of names.
#'
#' @family information gathering functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#'
#' @export
get_level_names <- function(gd) {
  UseMethod("get_level_names")
}


#' @rdname get_level_names
#' @export
get_level_names.geodimension <- function(gd) {
  names(gd$geolevel)
}

# get level geometries ---------------------------------------------------------------


#' Add a level to a dimension
#'
#' Add a level to a dimension. This level can then be related to other levels of
#' the dimension.
#'
#' @param gd A `geodimension` object.
#' @param level_name A string.
#'
#' @return A vector of names.
#'
#' @family information gathering functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
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
  names(gd$geolevel[[level_name]]$geometry)
}


# get level data ---------------------------------------------------------------


#' Add a level to a dimension
#'
#' Add a level to a dimension. This level can then be related to other levels of
#' the dimension.
#'
#' @param gd A `geodimension` object.
#' @param level_name A string.
#' @param inherited A boolean.
#'
#' @return A `tibble` object.
#'
#' @family information gathering functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#'
#' @export
get_level_data <- function(gd,
                           level_name = NULL,
                           inherited = FALSE) {
  UseMethod("get_level_data")
}


#' @rdname get_level_data
#' @export
get_level_data.geodimension <- function(gd,
                                        level_name = NULL,
                                        inherited = FALSE) {
  stopifnot(level_name %in% names(gd$geolevel))
  data <- gd$geolevel[[level_name]]$data
  if (inherited) {
    gd <- calculate_inherited_relationships(gd, level_name = level_name)
    key <- names(data)[1]
    for (rel in names(gd$relation[[level_name]])[-1]) {
      relation <- gd$relation[[level_name]][, c(level_name, rel)]
      names(relation)[2] <- paste(toupper(names(relation)[2]), names(relation)[2], sep = "_")
      data <- data %>%
        dplyr::left_join(relation, by = stats::setNames(level_name, key))
      rel_data <- gd$geolevel[[rel]]$data
      names(rel_data) <- paste(toupper(rel), names(rel_data), sep = "_")
      key_rel <- names(rel_data)[1]
      names(data)[length(names(data))] <- key_rel
      fk <- names(data)[length(names(data))]
      data <- data %>%
        dplyr::left_join(rel_data, by = stats::setNames(key_rel, fk))
    }
  }
  data
}


# get level layer ---------------------------------------------------------------


#' Add a level to a dimension
#'
#' Add a level to a dimension. This level can then be related to other levels of
#' the dimension.
#'
#' @param gd A `geodimension` object.
#' @param level_name A string.
#' @param attributes A boolean.
#' @param inherited A boolean.
#' @param geometry A string.
#'
#' @return A `geodimension`.
#'
#' @family information gathering functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#'
#' @export
get_level_layer <- function(gd,
                            level_name = NULL,
                            attributes = FALSE,
                            inherited = FALSE,
                            geometry = NULL) {
  UseMethod("get_level_layer")
}


#' @rdname get_level_layer
#' @export
get_level_layer.geodimension <- function(gd,
                                         level_name = NULL,
                                         attributes = FALSE,
                                         inherited = FALSE,
                                         geometry = NULL) {
  stopifnot(level_name %in% names(gd$geolevel))
  if (is.null(geometry)) {
    geometry <- names(gd$geolevel[[level_name]]$geometry)[1]
  } else {
    stopifnot(geometry %in% names(gd$geolevel[[level_name]]$geometry))
  }
  layer <- gd$geolevel[[level_name]]$geometry[[geometry]]
  if (attributes) {
    data <- gd %>%
      get_level_data(level_name = level_name, inherited = inherited)
    layer <- data %>%
      dplyr::left_join(layer, by = names(data)[1]) %>%
      sf::st_as_sf()
  }
  layer
}

# get higher level names ---------------------------------------------------------------

#' Add a level to a dimension
#'
#' Add a level to a dimension. This level can then be related to other levels of
#' the dimension.
#'
#' @param gd A `geodimension` object.
#' @param level_name A string.
#' @param indirect_levels A boolean.
#'
#' @return A vector of names.
#'
#' @family information gathering functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
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
  levels
}

