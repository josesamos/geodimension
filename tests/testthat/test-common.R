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
  file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
  layer_us_place <- sf::st_read(file, layer = "place", quiet = TRUE)
  layer_us_county <- sf::st_read(file, layer = "county", quiet = TRUE)

  expect_equal(
    get_geometry(layer_us_place),
    "point")

  expect_equal(
    get_geometry(layer_us_county),
    "polygon")

  expect_equal(
    check_key(layer_us_county, key = c("STATEFP", "NAME")),
    FALSE)

  expect_equal(
    check_key(layer_us_county, key = c("GEOID")),
    TRUE)

})


test_that("coordinates_to_geometry()", {
  file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
  layer_us_county <- sf::st_read(file, layer = "county", quiet = TRUE)
  layer_us_state <- sf::st_read(file, layer = "state", quiet = TRUE)

  us_state_point <-
    coordinates_to_geometry(layer_us_state)

  us_county_point <-
    coordinates_to_geometry(layer_us_county,
                            lon_lat = c("INTPTLON", "INTPTLAT"))


  expect_equal(
    get_geometry(us_state_point),
    "point")

  expect_equal(
    get_geometry(us_county_point),
    "point")

  expect_equal(
    nrow(us_county_point),
    nrow(layer_us_county))

  expect_equal(
    names(us_county_point),
    c(
      "STATEFP",
      "GEOID",
      "COUNTYFP",
      "COUNTYNS",
      "CLASSFP",
      "NAME",
      "NAMELSAD",
      "LSAD",
      "type",
      "Shape_Length",
      "Shape_Area",
      "ALAND",
      "AWATER",
      "geometry"
    )
  )

})

