context("test get_higher_level_names")

library(sf) # It has to be included even if it is not used directly.

test_that("get_higher_level_names works", {
  ln <- gd_us %>%
    get_higher_level_names(level_name = "state",
                           indirect_levels = TRUE)

  expect_equal(ln,
               c("division", "nation", "region"))

  ln <- gd_us %>%
    get_higher_level_names(level_name = "state")

  expect_equal(ln,
               c("division", "nation"))

})
