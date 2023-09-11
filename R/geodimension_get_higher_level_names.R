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
