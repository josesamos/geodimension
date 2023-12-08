test_that("geolevel()", {
  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = "GEOID")

  expect_equal(
    names(state),
    c("name", "key", "snake_case", "data", "geometry")
  )
  expect_equal(
    names(state$data),
    c("GEOID", "DIVISION", "REGION", "STATEFP", "STUSPS", "STATENS",
      "NAME", "Shape_Length", "Shape_Area", "ALAND", "AWATER", "INTPTLAT",
      "INTPTLON")
  )
  expect_equal(names(state$geometry$polygon),
               c("GEOID", "geom"))
  expect_equal(nrow(state$data),
               52)
  expect_equal(nrow(state$geometry$polygon),
               52)
})


test_that("geolevel()", {
  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = "geoid",
             snake_case = TRUE)

  expect_equal(
    names(state),
    c("name", "key", "snake_case", "data", "geometry")
  )
  expect_equal(
    names(state$data),
    c("geoid", "division", "region", "statefp", "stusps", "statens",
      "name", "shape_length", "shape_area", "aland", "awater", "intptlat",
      "intptlon")
  )
  expect_equal(names(state$geometry$polygon),
               c("geoid", "geom"))
  expect_equal(nrow(state$data),
               52)
  expect_equal(nrow(state$geometry$polygon),
               52)
})



test_that("geolevel()", {
  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             attributes = "NAME",
             key = "GEOID",
             snake_case = TRUE)

  expect_equal(
    names(state),
    c("name", "key", "snake_case", "data", "geometry")
  )
  expect_equal(
    names(state$data),
    c("geoid", "name")
  )
  expect_equal(names(state$geometry$polygon),
               c("geoid", "geom"))
  expect_equal(nrow(state$data),
               52)
  expect_equal(nrow(state$geometry$polygon),
               52)
})


test_that("snake_case_geolevel()", {
  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = "GEOID")
  state <- snake_case_geolevel(state)

  expect_equal(
    state$snake_case,
    TRUE
  )
  expect_equal(
    names(state$data),
    c("geoid", "division", "region", "statefp", "stusps", "statens",
      "name", "shape_length", "shape_area", "aland", "awater", "intptlat",
      "intptlon")
  )
  expect_equal(names(state$geometry$polygon),
               c("geoid", "geom"))
})


test_that("add_geometry()", {
  us_state_point <-
    coordinates_to_geometry(layer_us_state,
                            lon_lat = c("intptlon", "intptlat"))
  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = "geoid",
             snake_case = TRUE) |>
    add_geometry(layer = us_state_point,
                 layer_key = "stusps",
                 level_key = "stusps")

  expect_equal(
    names(state),
    c("name", "key", "snake_case", "data", "geometry")
  )
  expect_equal(
    names(state$data),
    c("geoid", "division", "region", "statefp", "stusps", "statens",
      "name", "shape_length", "shape_area", "aland", "awater", "intptlat",
      "intptlon")
  )
  expect_equal(names(state$geometry$polygon),
               c("geoid", "geom"))
  expect_equal(names(state$geometry$point),
               c("geoid", "geometry"))
  expect_equal(nrow(state$data),
               52)
  expect_equal(nrow(state$geometry$polygon),
               52)
  expect_equal(nrow(state$geometry$point),
               52)

})



test_that("add_geometry() and get_empty_geometry_instances()", {
  us_state_point <-
    coordinates_to_geometry(layer_us_state,
                            lon_lat = c("intptlon", "intptlat"))
  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = "geoid",
             snake_case = TRUE) |>
    add_geometry(layer = us_state_point)

  res <- state |> get_empty_geometry_instances(geometry = "point")

  expect_equal(
    names(state),
    c("name", "key", "snake_case", "data", "geometry")
  )
  expect_equal(
    names(state$data),
    c("geoid", "division", "region", "statefp", "stusps", "statens",
      "name", "shape_length", "shape_area", "aland", "awater", "intptlat",
      "intptlon")
  )
  expect_equal(names(state$geometry$polygon),
               c("geoid", "geom"))
  expect_equal(names(state$geometry$point),
               c("geoid", "geometry"))
  expect_equal(nrow(state$data),
               52)
  expect_equal(nrow(state$geometry$polygon),
               52)
  expect_equal(nrow(state$geometry$point),
               52)

  expect_equal(names(res),
               c("geoid", "division", "region", "statefp", "stusps", "statens",
                 "name", "shape_length", "shape_area", "aland", "awater", "intptlat",
                 "intptlon"))

  expect_equal(nrow(res),
               0)
})

test_that("complete_point_geometry()", {
  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = "geoid",
             snake_case = TRUE) |>
    complete_point_geometry()

  expect_equal(names(state$geometry$polygon),
               c("geoid", "geom"))

  expect_equal(names(state$geometry$point),
               c("geoid", "geom"))
})

test_that("complete_point_geometry()", {
  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = "geoid",
             snake_case = TRUE) |>
    complete_point_geometry()
  state <- state |>
    complete_point_geometry()

  expect_equal(names(state$geometry$polygon),
               c("geoid", "geom"))

  expect_equal(names(state$geometry$point),
               c("geoid", "geom"))
})

test_that("complete_point_geometry()", {
  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = "geoid",
             snake_case = TRUE) |>
    complete_point_geometry()

  state$geometry$point <- state$geometry$point[-3, ]

  state <- state |>
    complete_point_geometry()

  expect_equal(nrow(state$geometry$polygon),
               52)
  expect_equal(nrow(state$geometry$point),
               52)
})
