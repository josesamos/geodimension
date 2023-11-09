context("test get_unrelated_instances")

library(sf) # It has to be included even if it is not used directly.

test_that("get_unrelated_instances works", {
  region <-
    geolevel(name = "region",
             layer = layer_us_region,
             key = c("geoid"))
  division <-
    geolevel(name = "division",
             layer = layer_us_division,
             key = c("geoid"))
  gd <-
    geodimension(name = "gd_us",
                 level = region) |>
    add_level(division)

  gd <- gd |>
    relate_levels(lower_level_name = "division",
                  upper_level_name = "region",
                  by_geography = TRUE)

  ui <- gd |>
    get_unrelated_instances(lower_level_name = "division",
                            upper_level_name = "region")

  expect_equal(ui,
               structure(
                 list(
                   division_key = integer(0),
                   geoid = character(0),
                   divisionce = character(0),
                   affgeoid = character(0),
                   name = character(0),
                   lsad = character(0),
                   aland = numeric(0),
                   awater = numeric(0)
                 ),
                 row.names = integer(0),
                 class = c("tbl_df",
                           "tbl", "data.frame")
               ))
})
