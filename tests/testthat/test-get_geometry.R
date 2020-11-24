context("test get_geometry")

library(sf) # It has to be included even if it is not used directly.

test_that("get_geometry works", {
  geometry <- get_geometry(layer_us_region)
  expect_equal(geometry,
               c("polygon"))
})
