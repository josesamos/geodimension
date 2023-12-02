test_that("geolevel()", {
  region <-
    geolevel(name = "region",
             layer = layer_us_region,
             key = c("geoid"))

  expect_equal(
    attributes(region),
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
  expect_equal(
    names(region$data),
    c("region_key", "geoid", "regionce", "affgeoid", "name", "lsad",
      "aland", "awater")
  )
  expect_equal(names(region$geometry$polygon),
               c("region_key", "geom"))
})


test_that("add_geometry()", {
  us_state_point <-
    coordinates_to_geometry(layer_us_state,
                            lon_lat = c("intptlon", "intptlat"))

  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = c("geoid")) |>
    add_geometry(layer = us_state_point)

  expect_equal(names(state$geometry$polygon),
               c("state_key", "Shape"))

  expect_equal(names(state$geometry$point),
               c("state_key", "geometry"))
})
