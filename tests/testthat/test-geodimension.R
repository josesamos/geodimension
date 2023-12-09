test_that("geodimension()", {
  place <-
    geolevel(name = "place",
             layer = layer_us_place,
             key = c("GEOID"))
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
               c("GEOID", "STATEFP", "county_geoid", "PLACEFP", "PLACENS", "NAME",
                 "NAMELSAD", "LSAD", "type"))

  expect_equal(names(gd_2$geolevel$place$data),
               c("geoid", "statefp", "county_geoid", "placefp", "placens", "name",
                 "namelsad", "lsad", "type"))
})


test_that("add_level() and transform_crs()", {
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
               c("GEOID", "STATEFP", "NAME", "type"))

  expect_equal(names(gd_2$geolevel$county$data),
               c("geoid", "statefp", "name", "type"))

  expect_equal(r$input,
               "EPSG:3857")

  expect_equal(s$input,
               "EPSG:3857")

})


