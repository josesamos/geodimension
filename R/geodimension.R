#' `geodimension` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @param name A string, name of the dimension.
#' @param level A `geolevel`.
#'
#' @return A `geodimension` object.
#'
#' @keywords internal
new_geodimension <-
  function(name = NULL,
           level = NULL) {

    geolevel <- list()
    geolevel[[attr(level, "name")]] <- level

    relation <- list()
    data <- level$data[1]
    names(data) <- attr(level, "name")
    relation[[attr(level, "name")]] <- data

    geodimension <- list(geolevel = geolevel, relation = relation)

    structure(
      geodimension,
      name = name,
      class = "geodimension"
    )
  }

#' `geodimension` S3 class
#'
#' A `geodimension` object is created. A `geodimension` allows you to relate
#' levels. In addition to the name of the `geodimension` , a `level` has to be
#' given.
#'
#' @inheritParams new_geodimension
#'
#' @return A `geodimension` object.
#'
#' @family level association functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#' library(sf)
#'
#' region <-
#'   geolevel(name = "region",
#'            layer = layer_us_region,
#'            key = c("geoid"))
#'
#' gd <-
#'   geodimension(name = "gd_us",
#'                level = region)
#'
#' @export
geodimension <- function(name = NULL,
                         level = NULL) {
  new_geodimension(name, level)
}

# calculate inherited relationships ---------------------------------------

#' calculate inherited relationships
#'
#' Each level has explicitly defined relationships with other levels. For a
#' given level, all the relationships with the levels of the dimension, direct
#' and indirect, are obtained.
#'
#' @param gd A `geodimension` object.
#' @param level_name A string, name of the lower level.
#'
#' @keywords internal
calculate_inherited_relationships <- function(gd,
                                              level_name = NULL) {
  stopifnot(level_name %in% names(gd$geolevel))
  stopifnot(level_name %in% names(gd$relation))

  upper_level_names <- names(gd$relation[[level_name]])[-1]
  names_new <- upper_level_names
  already_considered <- NULL
  while (length(names_new) > 0) {
    already_considered <- c(already_considered, names_new)
    for (upper_level in names_new) {
      rel_names <- names(gd$relation[[upper_level]])[-1]
      rel_names <- generics::setdiff(rel_names, upper_level_names)
      for (rel in rel_names) {
        gd$relation[[level_name]] <- gd$relation[[level_name]] %>%
          dplyr::left_join(gd$relation[[upper_level]][, c(upper_level, rel)], by = upper_level)
      }
      upper_level_names <- names(gd$relation[[level_name]])[-1]
    }
    names_new <- generics::setdiff(upper_level_names, already_considered)
  }
  gd
}

