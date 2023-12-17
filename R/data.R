#' `gd_es`
#'
#' `geodimension` obtained from vector layers over Spain
#'
#' It includes the levels nucleus, municipality, agricultural region, province,
#' autonomous community and country.
#'
#' @examples
#' # Defined by:
#'
#' \donttest{
#' file <- system.file("extdata", "es_layers.gpkg", package = "geodimension")
#' layer_es_municipality <-
#'   sf::st_read(file, layer = "municipality", quiet = TRUE)
#' layer_es_province <-
#'   sf::st_read(file, layer = "province", quiet = TRUE)
#' layer_es_autonomous_community <-
#'   sf::st_read(file, layer = "autonomous_community", quiet = TRUE)
#'
#' municipality <-
#'   geolevel(
#'     name = "municipality",
#'     layer = layer_es_municipality,
#'     attributes = c("COD_PROV", "NAME", "CAPITAL", "REGION"),
#'     key = "COD_INE"
#'   ) |>
#'   add_geometry(layer = coordinates_to_geometry(
#'     layer_es_municipality,
#'     lon_lat = c("LON_ETRS89", "LAT_ETRS89"),
#'     crs = 4258
#'   ))
#'
#' agricultural_region <-
#'   geolevel(
#'     name = "agricultural_region",
#'     layer = get_level_layer(municipality),
#'     attributes = "REGION",
#'     key = c("COD_PROV", "REGION")
#'   ) |>
#'   complete_point_geometry()
#'
#' province <-
#'   geolevel(
#'     name = "province",
#'     layer = layer_es_province,
#'     attributes = c("COUNTRY", "COD_CA", "NAME", "CAPITAL_PROVINCE"),
#'     key = "COD_PROV"
#'   ) |>
#'   complete_point_geometry()
#'
#' autonomous_community <-
#'   geolevel(
#'     name = "autonomous_community",
#'     layer = layer_es_autonomous_community,
#'     attributes = c("COUNTRY", "NAME"),
#'     key = "COD_CA"
#'   ) |>
#'   complete_point_geometry()
#'
#' country <-
#'   geolevel(
#'     name = "country",
#'     layer = get_level_layer(autonomous_community),
#'     attributes = "COUNTRY",
#'     key = "COUNTRY"
#'   ) |>
#'   complete_point_geometry()
#'
#' gd_es <-
#'   geodimension(name = "gd_es",
#'                level = municipality,
#'                snake_case = TRUE) |>
#'   add_level(level = agricultural_region) |>
#'   add_level(level = province) |>
#'   add_level(level = autonomous_community) |>
#'   add_level(level = country)
#'
#' gd_es <- gd_es |>
#'   relate_levels(
#'     lower_level_name = "municipality",
#'     lower_level_attributes = "COD_PROV",
#'     upper_level_name = "province"
#'   ) |>
#'   relate_levels(
#'     lower_level_name = "municipality",
#'     lower_level_attributes = c("COD_PROV", "REGION"),
#'     upper_level_name = "agricultural_region"
#'   ) |>
#'   relate_levels(
#'     lower_level_name = "agricultural_region",
#'     lower_level_attributes = "COD_PROV",
#'     upper_level_name = "province"
#'   ) |>
#'   relate_levels(
#'     lower_level_name = "province",
#'     lower_level_attributes = "COD_CA",
#'     upper_level_name = "autonomous_community"
#'   ) |>
#'   relate_levels(
#'     lower_level_name = "autonomous_community",
#'     lower_level_attributes = "country",
#'     upper_level_name = "country"
#'   )
#' }
#'
#' @format A `geodimension`.
#' @source
#'   \url{https://centrodedescargas.cnig.es/CentroDescargas/},
#'   \url{https://www.mapa.gob.es/es/cartografia-y-sig/ide/descargas/agricultura/default.aspx}
"gd_es"


#' `gd_us`
#'
#' `geodimension` obtained from vector layers over USA.
#'
#' It includes the levels place, county, state, division, region and country.
#'
#' @examples
#' # Defined by:
#'
#' \donttest{
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
#' }
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
