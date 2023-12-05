test_that("geodimension()", {
  region <-
    geolevel(name = "region",
             layer = layer_us_region,
             key = c("geoid"))
  gd <-
    geodimension(name = "gd_us",
                 level = region)

  expect_equal(attributes(gd),
               list(names = c("name", "snake_case", "geolevel", "relation"),
                    class = "geodimension"))

  expect_equal(
    attributes(gd$geolevel$region),
    list(names = c("name", "key", "snake_case", "data", "geometry"
    ), class = "geolevel")
  )

  expect_equal(attributes(gd$relation),
               NULL)
})
