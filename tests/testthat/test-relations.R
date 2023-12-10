test_that("geodimension()", {
  file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
  layer_us_place <- sf::st_read(file, layer = "place", quiet = TRUE)
  layer_us_county <- sf::st_read(file, layer = "county", quiet = TRUE)
  layer_us_state <- sf::st_read(file, layer = "state", quiet = TRUE)

  place <-
    geolevel(name = "place",
             layer = layer_us_place,
             attributes = c("STATEFP", "county_geoid", "NAME", "type"),
             key = "GEOID")

  county <-
    geolevel(
      name = "county",
      layer = layer_us_county,
      attributes = c("STATEFP", "NAME", "type"),
      key = "GEOID"
    ) |>
    add_geometry(coordinates_to_geometry(layer_us_county,
                                         lon_lat = c("INTPTLON", "INTPTLAT")))

  gd_us_0 <-
    geodimension(name = "gd_us",
                 level = place) |>
    add_level(level = county)

  gd_us_0 <- gd_us_0 |>
    relate_levels(
      lower_level_name = "place",
      lower_level_attributes = "county_geoid",
      upper_level_name = "county"
    )

  gd_us_2 <- gd_us_0 |>
    relate_levels(
      lower_level_name = "place",
      upper_level_name = "county",
      by_geography = TRUE
    )

  gd_us_3 <- gd_us_0 |>
    relate_levels(
      lower_level_name = "place",
      lower_level_attributes = "county_geoid_geo",
      upper_level_name = "county",
      by_geography = TRUE
    )

  gd_us_4 <- gd_us_0 |>
    relate_levels(
      lower_level_name = "place",
      lower_level_attributes = "county_geoid",
      upper_level_name = "county",
      upper_level_key = "GEOID"
    )

  ui <- gd_us_0 |>
    get_unrelated_instances(
      lower_level_name = "place",
      upper_level_name = "county"
    )

  ui_2 <- gd_us_2 |>
    get_unrelated_instances(
      lower_level_name = "place",
      upper_level_name = "county"
    )

  gd_us_0$geolevel$place$data$county_geoid[2] <- NA
  ui_3 <- gd_us_0 |>
    get_unrelated_instances(
      lower_level_name = "place",
      upper_level_name = "county"
    )

  gd_us_5 <- gd_us_0 |>
    complete_relation_by_geography(
      lower_level_name = "place",
      upper_level_name = "county"
    )
  ui_4 <- gd_us_5 |>
    get_unrelated_instances(
      lower_level_name = "place",
      upper_level_name = "county"
    )

  expect_equal(gd_us_0$relation,
               list(place = list(
                 county = list(lower_fk = "county_geoid", upper_pk = "GEOID")
               )))

  expect_equal(gd_us_2$relation,
               list(place = list(
                 county = list(lower_fk = "fk_county_GEOID",
                               upper_pk = "GEOID")
               )))

  expect_equal(
    names(gd_us_2$geolevel$place$data),
    c(
      "GEOID",
      "STATEFP",
      "county_geoid",
      "NAME",
      "type",
      "fk_county_GEOID"
    )
  )

  expect_equal(gd_us_3$relation,
               list(place = list(
                 county = list(lower_fk = "county_geoid_geo",
                               upper_pk = "GEOID")
               )))

  expect_equal(
    names(gd_us_3$geolevel$place$data),
    c(
      "GEOID",
      "STATEFP",
      "county_geoid",
      "NAME",
      "type",
      "county_geoid_geo"
    )
  )

  expect_equal(gd_us_4$relation,
               list(place = list(
                 county = list(lower_fk = "county_geoid", upper_pk = "GEOID")
               )))

  expect_equal(ui,
               structure(
                 list(
                   GEOID = character(0),
                   STATEFP = character(0),
                   county_geoid = character(0),
                   NAME = character(0),
                   type = character(0)
                 ),
                 class = c("tbl_df",
                           "tbl", "data.frame"),
                 row.names = integer(0)
               ))

  expect_equal(ui_2,
               structure(
                 list(
                   GEOID = character(0),
                   STATEFP = character(0),
                   county_geoid = character(0),
                   NAME = character(0),
                   type = character(0),
                   fk_county_GEOID = character(0)
                 ),
                 class = c("tbl_df", "tbl",
                           "data.frame"),
                 row.names = integer(0)
               ))

  expect_equal(ui_3,
               structure(
                 list(
                   GEOID = "0100124",
                   STATEFP = "01",
                   county_geoid = NA_character_,
                   NAME = "Abbeville",
                   type = "city"
                 ),
                 row.names = c(NA,-1L),
                 class = c("tbl_df", "tbl", "data.frame")
               ))

  expect_equal(ui_4,
               ui)

})


test_that("get_higher_level_names()", {

ln_1 <- gd_us |>
  get_higher_level_names(level_name = "place")

ln_2 <- gd_us |>
  get_higher_level_names(level_name = "place", indirect_levels = TRUE)


expect_equal(ln_1,
             "county")

expect_equal(ln_2,
             c("county", "state", "division", "region", "country"))

})


test_that("select_levels()", {
  gd_us_2 <- gd_us |>
    select_levels(level_names = c("state", "county", "place", "region"))

  expect_equal(names(gd_us_2$geolevel),
               c("place", "county", "state", "region"))

  expect_equal(gd_us_2$relation,
               list(
                 place = list(county = list(
                   lower_fk = "county_geoid", upper_pk = "geoid"
                 )),
                 county = list(state = list(
                   lower_fk = "statefp", upper_pk = "statefp"
                 )),
                 state = list(
                   region = list(lower_fk = "fk_region_code",
                                 upper_pk = "region_code")
                 )
               ))

})


test_that("select_levels()", {
  gd_us_2 <- gd_us |>
    select_levels(level_names = c("state", "place", "region"))

  expect_equal(names(gd_us_2$geolevel),
               c("place", "state", "region"))

  expect_equal(gd_us_2$relation,
               list(
                 state = list(
                   region = list(lower_fk = "fk_region_code",
                                 upper_pk = "region_code")
                 ),
                 place = list(state = list(
                   lower_fk = "fk_statefp",
                   upper_pk = "statefp"
                 ))
               ))

})

test_that("select_levels()", {
  file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
  layer_us_state <- sf::st_read(file, layer = "state", quiet = TRUE)

  layer_us_state$country <- "USA"

  state <-
    geolevel(
      name = "state",
      layer = layer_us_state,
      attributes = c("DIVISION", "REGION", "STATEFP", "STUSPS", "NAME", "country"),
      key = "STATEFP"
    ) |>
    add_geometry(coordinates_to_geometry(layer_us_state,
                                         lon_lat = c("INTPTLON", "INTPTLAT")))

  division <-
    geolevel(
      name = "division",
      layer = us_division,
      attributes = c("country", "region_code", "division_name"),
      key = c("division_code", "country")
    ) |>
    add_geometry(layer = layer_us_state,
                 layer_key = c("DIVISION", "country")) |>
    complete_point_geometry()

  region <-
    geolevel(
      name = "region",
      layer = us_division,
      attributes = c("country", "region_name"),
      key = c("region_code", "country")
    ) |>
    add_geometry(layer = layer_us_state,
                 layer_key = c("REGION", "country")) |>
    complete_point_geometry()

  country <-
    geolevel(
      name = "country",
      layer = get_level_layer(region),
      attributes = "country",
      key = "country"
    ) |>
    complete_point_geometry()

  gd <-
    geodimension(name = "gd_us",
                 level = state,
                 snake_case = TRUE) |>
    add_level(level = division) |>
    add_level(level = region) |>
    add_level(level = country)

  gd <- gd |>
    relate_levels(
      lower_level_name = "state",
      lower_level_attributes = c("DIVISION", "country"),
      upper_level_name = "division"
    )
  gd <- gd |>
    relate_levels(
      lower_level_name = "division",
      lower_level_attributes = c("region_code", "country"),
      upper_level_name = "region"
    )
  gd <- gd |>
    relate_levels(
      lower_level_name = "region",
      lower_level_attributes = "country",
      upper_level_name = "country"
    )

  gd_2 <- gd |>
    select_levels(level_names = c("state", "region"))


  expect_equal(gd_2$relation,
               list(state = list(region = list(
                 lower_fk = c("fk_region_code",
                              "fk_region_country"),
                 upper_pk = c("region_code", "country")
               ))))

  expect_equal(
    names(gd_2$geolevel$state$data),
    c(
      "statefp",
      "division",
      "region",
      "stusps",
      "name",
      "country",
      "fk_region_code",
      "fk_region_country"
    )
  )
})
