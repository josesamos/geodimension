context("test get_level_data")

library(sf) # It has to be included even if it is not used directly.

test_that("get_level_data works", {
  gd <- gd_us %>%
    complete_relation_by_geography(lower_level_name = "state",
                                   upper_level_name = "division")

  ui <- gd_us %>%
    get_unrelated_instances(lower_level_name = "state",
                            upper_level_name = "division")

  expect_equal(attributes(ui),
               list(
                 names = c(
                   "state_key",
                   "geoid",
                   "region",
                   "division",
                   "statefp",
                   "statens",
                   "stusps",
                   "name",
                   "lsad",
                   "mtfcc",
                   "funcstat",
                   "aland",
                   "awater",
                   "intptlat",
                   "intptlon",
                   "shape_length",
                   "shape_area",
                   "geoid_data"
                 ),
                 row.names = 1L,
                 class = c("tbl_df", "tbl", "data.frame")
               ))
})
