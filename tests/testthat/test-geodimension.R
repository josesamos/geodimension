context("test geodimension")

library(sf) # It has to be included even if it is not used directly.

test_that("geodimension works", {
  region <-
    geolevel(name = "region",
             layer = layer_us_region,
             key = c("geoid"))
  gd <-
    geodimension(name = "gd_us",
                 level = region)

  expect_equal(attributes(gd),
               list(
                 names = c("geolevel", "relation"),
                 name = "gd_us",
                 class = "geodimension"
               ))

  expect_equal(
    attributes(gd$geolevel$region),
    list(
      names = c("data", "geometry"),
      name = "region",
      attributes = c(
        "geoid",
        "regionce",
        "affgeoid",
        "name",
        "lsad",
        "aland",
        "awater"
      ),
      key = "geoid",
      surrogate_key = "region_key",
      n_instances_data = 4L,
      class = "geolevel"
    )
  )

  expect_equal(attributes(gd$relation$region),
               list(
                 names = "region",
                 row.names = 1:4,
                 class = c("tbl_df", "tbl",
                           "data.frame")
               ))
})
