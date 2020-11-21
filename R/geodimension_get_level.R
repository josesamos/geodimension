

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
#' @family geodimension functions
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
#' @family geodimension functions
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
#' @family geodimension functions
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
    key <- names(data)[1]
    for (rel in names(gd$relation[[level_name]])[-1]) {
      data <- data %>%
        dplyr::left_join(gd$relation[[level_name]][, c(level_name, rel)], by = stats::setNames(level_name, key))
      fk <- names(data)[length(names(data))]
      rel_data <- gd$geolevel[[rel]]$data
      names(rel_data) <- paste(toupper(rel), names(rel_data), sep = "_")
      key <- names(rel_data)[1]
      data <- data %>%
        dplyr::left_join(rel_data, by = stats::setNames(key, fk), keep = TRUE) %>%
        dplyr::select(!fk)
    }
  }
  data
}


# get level ---------------------------------------------------------------


#' Add a level to a dimension
#'
#' Add a level to a dimension. This level can then be related to other levels of
#' the dimension.
#'
#' @param gd A `geodimension` object.
#' @param level A `geolevel`, level to add to the dimension.
#'
#' @return A `geodimension`.
#'
#' @family geodimension functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#'
#' @export
get_level <- function(gd,
                       level = NULL) {
  UseMethod("get_level")
}


#' @rdname get_level
#' @export
get_level.geodimension <- function(gd,
                                    level = NULL) {
  stopifnot(!(attr(level, "name") %in% names(gd$geolevel)))
  gd$geolevel[[attr(level, "name")]] <- level

  data <- level$data[1]
  names(data) <- attr(level, "name")
  gd$relation[[attr(level, "name")]] <- data
  gd
}
