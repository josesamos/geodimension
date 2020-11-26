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
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' ld <- gd_us %>%
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
      data <- data %>%
        dplyr::left_join(relation, by = stats::setNames(level_name, key))
      rel_data <- gd$geolevel[[rel]]$data
      if (add_prefix) {
        names(rel_data) <- paste(toupper(rel), names(rel_data), sep = "_")
      }
      key_rel <- names(rel_data)[1]
      names(data)[length(names(data))] <- key_rel
      fk <- names(data)[length(names(data))]
      data <- data %>%
        dplyr::left_join(rel_data, by = stats::setNames(key_rel, fk))
    }
  }
  data
}
