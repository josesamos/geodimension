# complete point geometry ----------------------------------------------------------

#' Complete point geometry
#'
#' In case of having the polygon geometry defined, it obtains the point geometry
#' from it.
#'
#' If the point geometry was already defined, if there are instances with this
#' geometry empty, it completes them.
#'
#' It is recommended to use a projected reference system to avoid errors in
#' calculations.
#'
#' @param gl A `geolevel` object.
#'
#' @return A `geolevel` object.
#'
#' @family level definition functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#' library(sf)
#'
#' state <-
#'   geolevel(name = "state",
#'            layer = layer_us_state,
#'            key = c("geoid")) %>%
#'   complete_point_geometry()
#'
#' @export
complete_point_geometry <- function(gl) {
  UseMethod("complete_point_geometry")
}


#' @rdname complete_point_geometry
#' @export
complete_point_geometry.geolevel <- function(gl) {
  stopifnot("polygon" %in% names(gl$geometry))
  if ("point" %in% names(gl$geometry)) {
    layer <- gl$geometry[["polygon"]][!(gl$data[[1]] %in% gl$geometry[["point"]][[1]]), ]
    # to avoid warning: make the assumption (that the attribute is constant throughout the geometry)
    sf::st_agr(layer) = "constant"
    rest <- layer %>%
      sf::st_point_on_surface() %>%
      sf::st_transform(crs = sf::st_crs(gl$geometry[["point"]]))
     gl$geometry[["point"]] <- gl$geometry[["point"]] %>%
      tibble::add_row(rest)
  } else {
    layer <- gl$geometry[["polygon"]]
    # to avoid warning: make the assumption (that the attribute is constant throughout the geometry)
    sf::st_agr(layer) = "constant"
    gl$geometry[["point"]] <- layer %>%
      sf::st_point_on_surface()
  }
  gl
}

