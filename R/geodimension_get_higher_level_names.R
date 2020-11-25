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
