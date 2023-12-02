context("test add_level")

test_that("add_level works", {
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

  expect_equal(attributes(gd),
               list(
                 names = c("geolevel", "relation"),
                 name = "gd_us",
                 class = "geodimension"
               ))

  expect_equal(
    attributes(gd$geolevel$division),
    list(
      names = c("data", "geometry"),
      name = "division",
      attributes = c(
        "geoid",
        "divisionce",
        "affgeoid",
        "name",
        "lsad",
        "aland",
        "awater"
      ),
      key = "geoid",
      surrogate_key = "division_key",
      n_instances_data = 9L,
      class = "geolevel"
    )
  )

  expect_equal(attributes(gd$relation$division),
               list(
                 names = "division",
                 row.names = 1:9,
                 class = c("tbl_df",
                           "tbl", "data.frame")
               ))
})
