context("test get_higher_level_names")

library(sf) # It has to be included even if it is not used directly.

test_that("get_higher_level_names works", {
  lg <- gd_us |>
    get_level_geometries(level_name = "state")

  expect_equal(lg,
               c("point", "polygon"))
})
