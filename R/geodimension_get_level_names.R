
# get level names ---------------------------------------------------------

#' Get level names
#'
#' Get the names of levels included in the `geodimension`.
#'
#' @param gd A `geodimension` object.
#'
#' @return A vector of names.
#'
#' @family information output functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' ln <- gd_us %>%
#'   get_level_names()
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
