context("test get_level_names")

library(sf) # It has to be included even if it is not used directly.

test_that("get_level_names works", {
  ln <- gd_us |>
    get_level_names()

  expect_equal(ln,
               c("city", "county", "division", "nation", "region", "state"))

})
