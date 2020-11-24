context("test geolevel")

library(sf) # It has to be included even if it is not used directly.

test_that("geolevel works", {
  region <-
    geolevel(name = "region",
             layer = layer_us_region,
             key = c("geoid"))

  expect_equal(
    attributes(region),
    list(
      names = c("data", "geometry"),
      name = "region",
      attributes = c(
        "geoid",
        "regionce",
        "affgeoid",
        "name",
        "lsad",
        "aland",
        "awater"
      ),
      key = "geoid",
      surrogate_key = "region_key",
      n_instances_data = 4L,
      class = "geolevel"
    )
  )
  expect_equal(
    names(region$data),
    c("region_key", "geoid", "regionce", "affgeoid", "name", "lsad",
      "aland", "awater")
  )
  expect_equal(names(region$geometry$polygon),
               c("region_key", "geom"))
})
