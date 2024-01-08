test_that("validate_names()", {
  expect_equal(validate_names(
    defined_names = c("a", "b", "c"),
    names = "a",
    concept = 'name',
    repeated = FALSE
  ),
  "a")
  expect_equal(
    validate_names(
      defined_names = c("a", "b", "c"),
      names = NULL,
      concept = 'name',
      repeated = FALSE
    ),
    c("a", "b", "c")
  )
  expect_equal(
    validate_names(
      defined_names = c("a", "b", "c"),
      names = c("a", "a"),
      concept = 'name',
      repeated = TRUE
    ),
    c("a", "a")
  )
  expect_equal({
    res <- tryCatch(
      validate_names(
        defined_names = c("a", "b", "c"),
        names = "d",
        concept = 'name',
        repeated = FALSE
      ),
      error = function(e)
        1
    )
    res
  },
  1)

})


test_that("get_geometry() and check_key()", {
  layer_us_place <- gd_us |>
    get_level_layer("place")

  layer_us_county <-
    dplyr::inner_join(
      get_level_data_geo(gd_us, "county"),
      get_level_layer(gd_us, "county"),
      by = c("geoid", "statefp", "name", "type")
    ) |>
    sf::st_as_sf()

  expect_equal(
    get_geometry(layer_us_place),
    "point")

  expect_equal(
    get_geometry(layer_us_county),
    "polygon")

  expect_equal(
    check_key(layer_us_county, key = c("statefp", "name")),
    FALSE)

  expect_equal(
    check_key(layer_us_county, key = c("geoid")),
    TRUE)

})


test_that("coordinates_to_geometry()", {
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

  us_state_point <-
    coordinates_to_geometry(layer_us_state)

  us_county_point <-
    coordinates_to_geometry(layer_us_county,
                            lon_lat = c("intptlon", "intptlat"))


  expect_equal(
    get_geometry(us_state_point),
    "point")

  expect_equal(
    get_geometry(us_county_point),
    "point")

  expect_equal(
    nrow(us_county_point),
    nrow(layer_us_county))

})

