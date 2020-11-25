

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




