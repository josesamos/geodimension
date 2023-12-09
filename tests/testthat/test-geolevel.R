test_that("geolevel()", {
  file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
  layer_us_state <- sf::st_read(file, layer = "state", quiet = TRUE)
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
  file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
  layer_us_state <- sf::st_read(file, layer = "state", quiet = TRUE)
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
  file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
  layer_us_state <- sf::st_read(file, layer = "state", quiet = TRUE)
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
  file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
  layer_us_state <- sf::st_read(file, layer = "state", quiet = TRUE)
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
  file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
  layer_us_state <- sf::st_read(file, layer = "state", quiet = TRUE)
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
  file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
  layer_us_state <- sf::st_read(file, layer = "state", quiet = TRUE)
  us_state_point <-
    coordinates_to_geometry(layer_us_state,
                            lon_lat = c("intptlon", "intptlat"))
  state <-
    geolevel(name = "state",
             layer = layer_us_state,
<<<<<<< HEAD
             key = "geoid",
             snake_case = TRUE) |>
=======
             key = c("geoid")) |>
>>>>>>> master
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
  file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
  layer_us_state <- sf::st_read(file, layer = "state", quiet = TRUE)
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
  file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
  layer_us_state <- sf::st_read(file, layer = "state", quiet = TRUE)
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
  file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
  layer_us_state <- sf::st_read(file, layer = "state", quiet = TRUE)
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


test_that("get_level_layer()", {
  file <- system.file("extdata", "us_layers.gpkg", package = "geodimension")
  layer_us_state <- sf::st_read(file, layer = "state", quiet = TRUE)
  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = "geoid",
             snake_case = TRUE) |>
    complete_point_geometry()

  state_1 <- state |>
    get_level_layer("polygon")

  state_2 <- state |>
    get_level_layer("point", only_key = TRUE)

  expect_equal(class(state_1),
               c("sf", "tbl_df", "tbl", "data.frame"))

  expect_equal(class(state_2),
               c("sf", "data.frame"))

  expect_equal(names(state_1),
               c("geoid", "division", "region", "statefp", "stusps", "statens",
                 "name", "shape_length", "shape_area", "aland", "awater", "intptlat",
                 "intptlon", "geom"))

  expect_equal(names(state_2),
               c("geoid", "geom"))

  expect_equal(nrow(state_1),
               52)

  expect_equal(nrow(state_2),
               52)

})
