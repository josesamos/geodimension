# unrelated instances -----------------------------------------------------

#' Get unrelated instances
#'
#' Given two levels between which an explicit relationship is defined, it
#' returns the lower-level instances that are not related to any higher-level
#' instances.
#'
#' @param gd A `geodimension` object.
#' @param lower_level_name A string, name of the lower level.
#' @param upper_level_name A string, name of the upper lever.
#'
#' @return A `tibble`.
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
#' division <-
#'   geolevel(name = "division",
#'            layer = layer_us_division,
#'            key = c("geoid"))
#'
#' gd <-
#'   geodimension(name = "gd_us",
#'                level = region) %>%
#'   add_level(division)
#'
#' gd <- gd %>%
#'   relate_levels(lower_level_name = "division",
#'                 upper_level_name = "region",
#'                 by_geography = TRUE)
#'
#' ui <- gd %>%
#'   get_unrelated_instances(lower_level_name = "division",
#'                           upper_level_name = "region")
#'
#' @export
get_unrelated_instances <- function(gd,
                                    lower_level_name = NULL,
                                    upper_level_name = NULL) {
  UseMethod("get_unrelated_instances")
}


#' @rdname get_unrelated_instances
#' @export
get_unrelated_instances.geodimension <- function(gd,
                                                 lower_level_name = NULL,
                                                 upper_level_name = NULL) {
  stopifnot(lower_level_name %in% names(gd$geolevel))
  stopifnot(upper_level_name %in% names(gd$geolevel))
  stopifnot(upper_level_name %in% names(gd$relation[[lower_level_name]]))

  unrelated <- gd$relation[[lower_level_name]][[lower_level_name]][is.na(gd$relation[[lower_level_name]][[upper_level_name]])]
  gd$geolevel[[lower_level_name]]$data[unrelated, ]
}

