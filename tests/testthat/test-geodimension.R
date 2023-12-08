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


