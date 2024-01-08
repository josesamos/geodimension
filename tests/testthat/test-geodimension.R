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
             key = c("geoid"))
  gd <-
    geodimension(name = "gd_us",
                 level = place)

  gd_2 <-
    geodimension(name = "gd_us",
                 level = place,
                 snake_case = TRUE)

  expect_equal(attributes(gd),
               list(names = c("name", "snake_case", "geolevel", "relation"),
                    class = "geodimension"))

  expect_equal(
    attributes(gd$geolevel$place),
    list(names = c("name", "key", "snake_case", "data", "geometry"
    ), class = "geolevel")
  )

  expect_equal(attributes(gd$relation),
               NULL)

  expect_equal(names(gd$geolevel$place$data),
               c("geoid", "statefp", "county_geoid", "name", "type"))

  expect_equal(names(gd_2$geolevel$place$data),
               c("geoid", "statefp", "county_geoid", "name", "type"))
})


test_that("add_level() and transform_crs()", {
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

  gd <-
    geodimension(name = "gd_us",
                 level = place) |>
    add_level(level = county)

  gd_2 <-
    geodimension(name = "gd_us",
                 level = place,
                 snake_case = TRUE) |>
    add_level(level = county)

  gd_3 <- gd |>
    transform_crs(crs = 3857)

  r <- sf::st_crs(gd_3$geolevel$place$geometry$point)
  s <- sf::st_crs(gd_3$geolevel$county$geometry$polygon)


  expect_equal(attributes(gd),
               list(names = c("name", "snake_case", "geolevel", "relation"),
                    class = "geodimension"))

  expect_equal(
    attributes(gd$geolevel$county),
    list(names = c("name", "key", "snake_case", "data", "geometry"
    ), class = "geolevel")
  )

  expect_equal(attributes(gd$relation),
               NULL)

  expect_equal(names(gd$geolevel$county$data),
               c("geoid", "statefp", "name", "type"))

  expect_equal(names(gd_2$geolevel$county$data),
               c("geoid", "statefp", "name", "type"))

  expect_equal(r$input,
               "EPSG:3857")

  expect_equal(s$input,
               "EPSG:3857")

})


