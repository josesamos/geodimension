test_that("geodimension()", {
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

  gd_us <-
    geodimension(name = "gd_us",
                 level = place) |>
    add_level(level = county)

  gd_us <- gd_us |>
    relate_levels(
      lower_level_name = "place",
      lower_level_attributes = "county_geoid",
      upper_level_name = "county"
    )

  gd_us_2 <- gd_us |>
    relate_levels(
      lower_level_name = "place",
      upper_level_name = "county",
      by_geography = TRUE
    )

  gd_us_3 <- gd_us |>
    relate_levels(
      lower_level_name = "place",
      lower_level_attributes = "county_geoid_geo",
      upper_level_name = "county",
      by_geography = TRUE
    )

  gd_us_4 <- gd_us |>
    relate_levels(
      lower_level_name = "place",
      lower_level_attributes = "county_geoid",
      upper_level_name = "county",
      upper_level_key = "GEOID"
    )

  expect_equal(gd_us$relation,
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

})

