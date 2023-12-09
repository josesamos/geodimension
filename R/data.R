#' `gd_us`
#'
#' `geodimension` obtained from vector layers over USA.
#'
#' It includes the levels place, county, state, division, region and country.
#'
#' @examples
#' # Defined by:
#'
#' file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
#' layer_us_place <- sf::st_read(file, layer = "place", quiet = TRUE)
#' layer_us_county <- sf::st_read(file, layer = "county", quiet = TRUE)
#' layer_us_state <- sf::st_read(file, layer = "state", quiet = TRUE)
#'
#' place <-
#'   geolevel(name = "place",
#'            layer = layer_us_place,
#'            attributes = c("STATEFP", "county_geoid", "NAME", "type"),
#'            key = "GEOID")
#'
#' county <-
#'   geolevel(
#'     name = "county",
#'     layer = layer_us_county,
#'     attributes = c("STATEFP", "NAME", "type"),
#'     key = "GEOID"
#'   ) |>
#'   add_geometry(coordinates_to_geometry(layer_us_county,
#'                                        lon_lat = c("INTPTLON", "INTPTLAT")))
#'
#' state <-
#'   geolevel(
#'     name = "state",
#'     layer = layer_us_state,
#'     attributes = c("DIVISION", "REGION", "STATEFP", "STUSPS", "NAME"),
#'     key = "STATEFP"
#'   ) |>
#'   add_geometry(coordinates_to_geometry(layer_us_state,
#'                                        lon_lat = c("INTPTLON", "INTPTLAT")))
#'
#' division <-
#'   geolevel(
#'     name = "division",
#'     layer = us_division,
#'     attributes = c("country", "region_code", "division_name"),
#'     key = "division_code"
#'   ) |>
#'   add_geometry(layer = layer_us_state,
#'                layer_key = "DIVISION") |>
#'   complete_point_geometry()
#'
#' region <-
#'   geolevel(
#'     name = "region",
#'     layer = us_division,
#'     attributes = c("country", "region_name"),
#'     key = "region_code"
#'   ) |>
#'   add_geometry(layer = layer_us_state,
#'                layer_key = "REGION") |>
#'   complete_point_geometry()
#'
#' country <-
#'   geolevel(
#'     name = "country",
#'     layer = get_level_layer(region),
#'     attributes = "country",
#'     key = "country"
#'   ) |>
#'   complete_point_geometry()
#'
#' gd_us <-
#'   geodimension(name = "gd_us",
#'                level = place,
#'                snake_case = TRUE) |>
#'   add_level(level = county) |>
#'   add_level(level = state) |>
#'   add_level(level = division) |>
#'   add_level(level = region) |>
#'   add_level(level = country)
#'
#' gd_us <- gd_us |>
#'   relate_levels(
#'     lower_level_name = "place",
#'     lower_level_attributes = "county_geoid",
#'     upper_level_name = "county"
#'   ) |>
#'   relate_levels(
#'     lower_level_name = "county",
#'     lower_level_attributes = "STATEFP",
#'     upper_level_name = "state"
#'   ) |>
#'   relate_levels(
#'     lower_level_name = "state",
#'     lower_level_attributes = "DIVISION",
#'     upper_level_name = "division"
#'   ) |>
#'   relate_levels(
#'     lower_level_name = "division",
#'     lower_level_attributes = "region_code",
#'     upper_level_name = "region"
#'   ) |>
#'   relate_levels(
#'     lower_level_name = "region",
#'     lower_level_attributes = "country",
#'     upper_level_name = "country"
#'   )
#'
#' @format A `geodimension`.
#' @source
#'   \url{https://www.census.gov}
"gd_us"


#' `us_division`
#'
#' US Divisions and Regions (code and name).
#'
#' @format A `tibble` object.
"us_division"
