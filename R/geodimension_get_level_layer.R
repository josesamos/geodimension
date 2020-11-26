# get level layer ---------------------------------------------------------------

#' Get level layer
#'
#' Get a geographic layer associated with a level. We can select the geometry
#' and, using boolean parameters, which attributes are included in the layer's
#' table: only the attributes that make up the key, the subrogate key, inherited
#' attributes.
#'
#' In case of inheriting attributes from other levels, in the table, these can
#' have as a prefix the name of the level in uppercase.
#'
#' @param gd A `geodimension` object.
#' @param level_name A string.
#' @param geometry A string.
#' @param only_key A boolean.
#' @param surrogate_key A boolean.
#' @param inherited A boolean.
#' @param add_prefix A boolean.
#'
#' @return A `sf` object.
#'
#' @family information output functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#' library(sf)
#'
#' ll <- gd_us %>%
#'   get_level_layer(level_name = "division",
#'                   only_key = TRUE,
#'                   surrogate_key = TRUE)
#'
#' @export
get_level_layer <- function(gd,
                            level_name = NULL,
                            geometry = NULL,
                            only_key = FALSE,
                            surrogate_key = FALSE,
                            inherited = FALSE,
                            add_prefix = TRUE) {
  UseMethod("get_level_layer")
}


#' @rdname get_level_layer
#' @export
get_level_layer.geodimension <- function(gd,
                                         level_name = NULL,
                                         geometry = NULL,
                                         only_key = FALSE,
                                         surrogate_key = FALSE,
                                         inherited = FALSE,
                                         add_prefix = TRUE) {
  stopifnot(level_name %in% names(gd$geolevel))
  if (is.null(geometry)) {
    geometry <- names(gd$geolevel[[level_name]]$geometry)[1]
  } else {
    stopifnot(geometry %in% names(gd$geolevel[[level_name]]$geometry))
  }
  layer <- gd$geolevel[[level_name]]$geometry[[geometry]]
  data <- gd %>%
    get_level_data(level_name = level_name, inherited = inherited, add_prefix = add_prefix)
  if (only_key) {
    data <- data %>%
      dplyr::select(c(attr(gd$geolevel[[level_name]], "surrogate_key"), attr(gd$geolevel[[level_name]], "key")))
  }
  if (surrogate_key) {
    sel <- NULL
  } else {
    sel <- c(1)
  }

  data %>%
    dplyr::left_join(layer, by = names(data)[1]) %>%
    dplyr::select(!sel) %>%
    sf::st_as_sf()
}
