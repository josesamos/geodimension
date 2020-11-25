# unrelated instances -----------------------------------------------------

#' Relate levels in a dimension
#'
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
#'
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

