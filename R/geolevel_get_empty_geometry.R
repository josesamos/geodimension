# empty geometry ----------------------------------------------------------

#' Get empty geometry instances
#'
#' Get the instances of the data table that do not have associated geometry for
#' the specified geometry type.
#'
#' @param gl A `geolevel` object.
#' @param geometry A string, type of geometry of the layer.
#'
#' @return A `tibble`.
#'
#' @family level definition functions
#'
#' @examples
#' us_state_point <-
#'   coordinates_to_geometry(layer_us_state,
#'                           lon_lat = c("intptlon", "intptlat"))
#'
#' state <-
#'   geolevel(name = "state",
#'            layer = layer_us_state,
#'            key = c("geoid")) |>
#'   add_geometry(layer = us_state_point)
#'
#' empty_geometry_instances <- state |>
#'   get_empty_geometry_instances(geometry = "point")
#'
#' @export
get_empty_geometry_instances <- function(gl,
                                         geometry = NULL) {
  UseMethod("get_empty_geometry_instances")
}


#' @rdname get_empty_geometry_instances
#' @export
get_empty_geometry_instances.geolevel <- function(gl,
                                                  geometry = NULL) {
  stopifnot(geometry %in% names(gl$geometry))
  if (is.null(geometry)) {
    geometry <- names(gl$geometry)[1]
  }
  gl$data[!(gl$data[[1]] %in% gl$geometry[[geometry]][[1]]), ]
}

