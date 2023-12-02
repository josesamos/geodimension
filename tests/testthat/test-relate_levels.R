context("test relate_levels")

test_that("relate_levels works", {
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

  expect_equal(gd$relation$division,
               structure(
                 list(
                   division = 1:9,
                   region = c(1L, 1L, 2L, 2L, 3L,
                              3L, 3L, 4L, 4L)
                 ),
                 row.names = c(NA,-9L),
                 class = c("tbl_df",
                           "tbl", "data.frame")
               ))
})
