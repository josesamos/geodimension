# complete relation by geography -----------------------------------------------------

#' Complete relation by geography
#'
#' Two levels can be related by attributes or by geography (if the upper level
#' has polygon-type geometry). Once related, if there are unrelated instances,
#' you can try to relate those instances using this function, which considers
#' alternative geographic relationships.
#'
#' For example, if the lower level has associated point and polygon geometries,
#' only point geometry is considered to establish the initial relationship.
#' Polygon geometry is also considered in this function.
#'
#' It does not necessarily succeed trying to relate the instances.
#'
#' @param gd A `geodimension` object.
#' @param lower_level_name A string, name of the lower level.
#' @param upper_level_name A string, name of the upper lever.
#'
#' @return A `geodimension` object.
#'
#' @family level association functions
#'
#' @examples
#' ui <- gd_us |>
#'   get_unrelated_instances(lower_level_name = "state",
#'                           upper_level_name = "division")
#'
#' gd <- gd_us |>
#'   complete_relation_by_geography(lower_level_name = "state",
#'                           upper_level_name = "division")
#'
#' @export
complete_relation_by_geography <- function(gd,
                                           lower_level_name = NULL,
                                           upper_level_name = NULL) {
  UseMethod("complete_relation_by_geography")
}


#' @rdname complete_relation_by_geography
#' @export
complete_relation_by_geography.geodimension <- function(gd,
                                                        lower_level_name = NULL,
                                                        upper_level_name = NULL) {
  stopifnot(lower_level_name %in% names(gd$geolevel))
  stopifnot(upper_level_name %in% names(gd$geolevel))
  stopifnot(upper_level_name %in% names(gd$relation[[lower_level_name]]))

  upper_geom <- names(gd$geolevel[[upper_level_name]]$geometry)
  lower_geom <- names(gd$geolevel[[lower_level_name]]$geometry)

  unrelated <- gd$relation[[lower_level_name]][[lower_level_name]][is.na(gd$relation[[lower_level_name]][[upper_level_name]])]
  strategy <- 1

  while (length(unrelated) > 0 & strategy < 3) {
    if ("polygon" %in% upper_geom) {
      if (strategy == 1) {
        if ("point" %in% lower_geom) {
          lower_geom_sel <- "point"
        } else if ("line" %in% lower_geom) {
          lower_geom_sel <- "line"
        } else {
          lower_geom_sel <- "polygon"
        }
      } else {
        if ("polygon" %in% lower_geom) {
          lower_geom_sel <- "polygon"
        } else if ("line" %in% lower_geom) {
          lower_geom_sel <- "line"
        } else {
          lower_geom_sel <- "point"
        }
      }
      layer <- gd$geolevel[[lower_level_name]]$geometry[[lower_geom_sel]]
      layer <- layer[layer[[1]] %in% unrelated, ]
      if (lower_geom_sel != "point") {
        # to avoid warning: make the assumption (that the attribute is constant throughout the geometry)
        sf::st_agr(layer) = "constant"
        layer <- sf::st_point_on_surface(layer)
      }

      res <- sf::st_join(layer, gd$geolevel[[upper_level_name]]$geometry[["polygon"]], join = sf::st_within) |>
        sf::st_drop_geometry()
      names(res) <- c(lower_level_name, upper_level_name)

      multiplicity_n_1 <- (nrow(res) == length(unrelated))
      stopifnot(multiplicity_n_1)

      for (i in unrelated) {
        if (!is.na(res[res[[1]] == i, upper_level_name])) {
          gd$relation[[lower_level_name]][gd$relation[[lower_level_name]][[1]] == i, upper_level_name] <-
            res[res[[1]] == i, upper_level_name]
        }
      }
    }
    unrelated <- gd$relation[[lower_level_name]][[lower_level_name]][is.na(gd$relation[[lower_level_name]][[upper_level_name]])]
    strategy <- strategy + 1
  }
  gd
}


