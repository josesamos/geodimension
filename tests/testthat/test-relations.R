test_that("geodimension()", {
  layer_us_place <- gd_us |>
    get_level_layer("place")

  layer_us_county <-
    dplyr::inner_join(
      get_level_data_geo(gd_us, "county"),
      get_level_layer(gd_us, "county"),
      by = c("geoid", "statefp", "name", "type")
    ) |>
    sf::st_as_sf()

  layer_us_state <-
    dplyr::inner_join(
      get_level_data_geo(gd_us, "state"),
      get_level_layer(gd_us, "state"),
      by = c("statefp", "division", "region", "stusps", "name")
    ) |>
    sf::st_as_sf()

  place <-
    geolevel(name = "place",
             layer = layer_us_place,
             attributes = c("statefp", "county_geoid", "name", "type"),
             key = "geoid")

  county <-
    geolevel(
      name = "county",
      layer = layer_us_county,
      attributes = c("statefp", "name", "type"),
      key = "geoid"
    ) |>
    add_geometry(coordinates_to_geometry(layer_us_county,
                                         lon_lat = c("intptlon", "intptlat")))

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
      upper_level_key = "geoid"
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
                 county = list(lower_fk = "county_geoid", upper_pk = "geoid")
               )))

  expect_equal(gd_us_2$relation,
               list(place = list(
                 county = list(lower_fk = "fk_county_geoid",
                               upper_pk = "geoid")
               )))

  expect_equal(
    names(gd_us_2$geolevel$place$data),
    c(
      "geoid",
      "statefp",
      "county_geoid",
      "name",
      "type",
      "fk_county_geoid"
    )
  )

  expect_equal(gd_us_3$relation,
               list(place = list(
                 county = list(lower_fk = "county_geoid_geo",
                               upper_pk = "geoid")
               )))

  expect_equal(
    names(gd_us_3$geolevel$place$data),
    c(
      "geoid",
      "statefp",
      "county_geoid",
      "name",
      "type",
      "county_geoid_geo"
    )
  )

  expect_equal(gd_us_4$relation,
               list(place = list(
                 county = list(lower_fk = "county_geoid", upper_pk = "geoid")
               )))

  expect_equal(ui,
               structure(
                 list(
                   geoid = character(0),
                   statefp = character(0),
                   county_geoid = character(0),
                   name = character(0),
                   type = character(0)
                 ),
                 class = c("tbl_df",
                           "tbl", "data.frame"),
                 row.names = integer(0)
               ))

  expect_equal(ui_2,
               structure(
                 list(
                   geoid = character(0),
                   statefp = character(0),
                   county_geoid = character(0),
                   name = character(0),
                   type = character(0),
                   fk_county_geoid = character(0)
                 ),
                 class = c("tbl_df", "tbl",
                           "data.frame"),
                 row.names = integer(0)
               ))

  expect_equal(ui_3,
               structure(
                 list(
                   geoid = "0100124",
                   statefp = "01",
                   county_geoid = NA_character_,
                   name = "Abbeville",
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
  layer_us_state <-
    dplyr::inner_join(
      get_level_data_geo(gd_us, "state"),
      get_level_layer(gd_us, "state"),
      by = c("statefp", "division", "region", "stusps", "name")
    ) |>
    sf::st_as_sf()

  layer_us_state$country <- "USA"

  state <-
    geolevel(
      name = "state",
      layer = layer_us_state,
      attributes = c("division", "region", "statefp", "stusps", "name", "country"),
      key = "statefp"
    ) |>
    add_geometry(coordinates_to_geometry(layer_us_state,
                                         lon_lat = c("intptlon", "intptlat")))

  division <-
    geolevel(
      name = "division",
      layer = us_division,
      attributes = c("country", "region_code", "division_name"),
      key = c("division_code", "country")
    ) |>
    add_geometry(layer = layer_us_state,
                 layer_key = c("division", "country")) |>
    complete_point_geometry()

  region <-
    geolevel(
      name = "region",
      layer = us_division,
      attributes = c("country", "region_name"),
      key = c("region_code", "country")
    ) |>
    add_geometry(layer = layer_us_state,
                 layer_key = c("region", "country")) |>
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
      lower_level_attributes = c("division", "country"),
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
