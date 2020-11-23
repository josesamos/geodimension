context("test add_geometry")

library(sf) # It has to be included even if it is not used directly.

test_that("add_geometry works", {
  us_state_polygon <-
    get_level_layer(gd_us_city, level_name = "state", attributes = TRUE, geometry = "polygon")
  us_state_point <-
    get_level_layer(gd_us_city, level_name = "state", attributes = TRUE, geometry = "point")

  state <-
    geolevel(name = "state",
             layer = us_state_polygon,
             key = c("geoid")) %>%
    add_geometry(layer = us_state_point)

  expect_equal(names(state$geometry$polygon),
               c("state_key", "geom"))

  expect_equal(names(state$geometry$point),
               c("state_key", "geom"))
})
