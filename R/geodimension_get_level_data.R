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
