#' Relate levels in a dimension
#'
#'
#' @param gd A `geodimension` object.
#' @param lower_level_name A string, name of the lower level.
#' @param lower_level_attributes A vector of attribute names.
#' @param upper_level_name A string, name of the upper lever.
#' @param upper_level_key A vector of attribute names.
#' @param by_geography A boolean.
#'
#' @return A `geodimension`.
#'
#' @family geodimension functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#'
#' @export
relate_levels <- function(gd,
                          lower_level_name = NULL,
                          lower_level_attributes = NULL,
                          upper_level_name = NULL,
                          upper_level_key = NULL,
                          by_geography = FALSE) {
  UseMethod("relate_levels")
}


#' @rdname relate_levels
#' @export
relate_levels.geodimension <- function(gd,
                                       lower_level_name = NULL,
                                       lower_level_attributes = NULL,
                                       upper_level_name = NULL,
                                       upper_level_key = NULL,
                                       by_geography = FALSE) {
  stopifnot(lower_level_name %in% names(gd$geolevel))
  stopifnot(upper_level_name %in% names(gd$geolevel))
  lower_level_attributes <- unique(lower_level_attributes)
  if (is.null(upper_level_key)) {
    upper_level_key <- attr(gd$geolevel[[upper_level_name]], "key")
  } else {
    upper_level_key <- unique(upper_level_key)
    upper_level_key_is_a_key <-
      (nrow(gd$geolevel[[upper_level_name]]$data) == nrow(unique(gd$geolevel[[upper_level_name]]$data[, upper_level_key])))
    stopifnot(upper_level_key_is_a_key)
  }
  if (!is.null(lower_level_attributes)) {
    stopifnot(length(lower_level_attributes) == length(upper_level_key))
    stopifnot(lower_level_attributes %in% attr(gd$geolevel[[lower_level_name]], "attributes"))
    stopifnot(upper_level_key %in% attr(gd$geolevel[[upper_level_name]], "attributes"))
  }
  stopifnot(!(upper_level_name %in% names(gd$relation[[lower_level_name]])))

  if (by_geography) {
    stopifnot(is.null(lower_level_attributes))
    stopifnot("polygon" %in% names(gd$geolevel[[upper_level_name]]$geometry))
    lower_geom <- names(gd$geolevel[[lower_level_name]]$geometry)
    if ("point" %in% lower_geom) {
      lower_geom <- "point"
    } else if ("line" %in% lower_geom) {
      lower_geom <- "line"
    }
    layer <- gd$geolevel[[lower_level_name]]$geometry[[lower_geom]]
    if (lower_geom == "polygon") {
      # to avoid warning: make the assumption (that the attribute is constant throughout the geometry)
      sf::st_agr(layer) = "constant"
      layer <- sf::st_point_on_surface(layer)
    }

    res <- sf::st_join(layer, gd$geolevel[[upper_level_name]]$geometry[["polygon"]], join = sf::st_within) %>%
      sf::st_drop_geometry()
    names(res) <- c(lower_level_name, upper_level_name)

    multiplicity_n_1 <- nrow(res) == nrow(gd$relation[[lower_level_name]])
    stopifnot(multiplicity_n_1)

    gd$relation[[lower_level_name]] <-
      gd$relation[[lower_level_name]] %>%
      dplyr::left_join(res, by = lower_level_name)

  } else if (attr(gd$geolevel[[upper_level_name]], "n_instances_data") == 1) {
    stopifnot(is.null(lower_level_attributes))
    gd$relation[[lower_level_name]] <- gd$relation[[lower_level_name]] %>%
      tibble::add_column(!!upper_level_name := gd$relation[[upper_level_name]][[upper_level_name]])
  } else {
    lower_data <-
      gd$geolevel[[lower_level_name]]$data[, c(attr(gd$geolevel[[lower_level_name]], "surrogate_key"),
                                               lower_level_attributes)]
    names(lower_data) <-
      c(attr(gd$geolevel[[lower_level_name]], "surrogate_key"),
        upper_level_key)
    upper_data <-
      gd$geolevel[[upper_level_name]]$data[, c(attr(gd$geolevel[[upper_level_name]], "surrogate_key"),
                                               upper_level_key)]
    lower_data <- lower_data %>%
      dplyr::left_join(upper_data, by = upper_level_key) %>%
      dplyr::select(c(
        attr(gd$geolevel[[lower_level_name]], "surrogate_key"),
        attr(gd$geolevel[[upper_level_name]], "surrogate_key")
      ))
    names(lower_data) <- c(lower_level_name, upper_level_name)

    multiplicity_n_1 <- nrow(lower_data) == nrow(gd$relation[[lower_level_name]])
    stopifnot(multiplicity_n_1)

    gd$relation[[lower_level_name]] <- gd$relation[[lower_level_name]] %>%
      dplyr::left_join(lower_data, by = lower_level_name)

  }
  gd
}


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
#' @family geodimension functions
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

# complete relation by geography -----------------------------------------------------

#' complete relation by geography
#'
#'
#' @param gd A `geodimension` object.
#' @param lower_level_name A string, name of the lower level.
#' @param upper_level_name A string, name of the upper lever.
#'
#' @return A `geodimension` object.
#'
#' @family geodimension functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
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

      res <- sf::st_join(layer, gd$geolevel[[upper_level_name]]$geometry[["polygon"]], join = sf::st_within) %>%
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



# calculate inherited relationships ---------------------------------------


#' calculate inherited relationships
#'
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
