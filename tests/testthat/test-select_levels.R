context("test select_levels")

library(sf) # It has to be included even if it is not used directly.

test_that("select_levels works", {
  gds <- gd_us %>%
    select_levels(level_names = c("division", "region", "nation"))

  expect_equal(names(gds$geolevel),
               c("region", "division", "nation"))

  expect_equal(names(gds$relation),
               c("region", "division", "nation"))
})
