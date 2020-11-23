context("test get_geometry")

library(sf) # It has to be included even if it is not used directly.

test_that("get_geometry works", {
  us_region <-
    get_level_layer(gd_us_city, level_name = "region", attributes = TRUE)

  geometry <- get_geometry(us_region)
  expect_equal(geometry,
               c("polygon"))
})
