test_that("geolevel()", {
  layer_us_state <-
    dplyr::inner_join(
      get_level_data_geo(gd_us, "state"),
      get_level_layer(gd_us, "state"),
      by = c("statefp", "division", "region", "stusps", "name")
    ) |>
    sf::st_as_sf()

  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = "statefp")

  expect_equal(
    names(state),
    c("name", "key", "snake_case", "data", "geometry")
  )
  expect_equal(
    names(state$data),
    c("statefp", "division", "region", "stusps", "name", "intptlon",
      "intptlat")
  )
  expect_equal(names(state$geometry$polygon),
               c("statefp", "geom"))
  expect_equal(nrow(state$data),
               52)
  expect_equal(nrow(state$geometry$polygon),
               52)
})


test_that("geolevel()", {
  layer_us_state <-
    dplyr::inner_join(
      get_level_data_geo(gd_us, "state"),
      get_level_layer(gd_us, "state"),
      by = c("statefp", "division", "region", "stusps", "name")
    ) |>
    sf::st_as_sf()

  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = "statefp",
             snake_case = TRUE)

  expect_equal(
    names(state),
    c("name", "key", "snake_case", "data", "geometry")
  )
  expect_equal(
    names(state$data),
    c("statefp", "division", "region", "stusps", "name", "intptlon",
      "intptlat")
  )
  expect_equal(names(state$geometry$polygon),
               c("statefp", "geom"))
  expect_equal(nrow(state$data),
               52)
  expect_equal(nrow(state$geometry$polygon),
               52)
})



test_that("geolevel()", {
  layer_us_state <-
    dplyr::inner_join(
      get_level_data_geo(gd_us, "state"),
      get_level_layer(gd_us, "state"),
      by = c("statefp", "division", "region", "stusps", "name")
    ) |>
    sf::st_as_sf()

  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             attributes = "name",
             key = "statefp",
             snake_case = TRUE)

  expect_equal(
    names(state),
    c("name", "key", "snake_case", "data", "geometry")
  )
  expect_equal(
    names(state$data),
    c("statefp", "name")
  )
  expect_equal(names(state$geometry$polygon),
               c("statefp", "geom"))
  expect_equal(nrow(state$data),
               52)
  expect_equal(nrow(state$geometry$polygon),
               52)
})


test_that("snake_case_geolevel()", {
  layer_us_state <-
    dplyr::inner_join(
      get_level_data_geo(gd_us, "state"),
      get_level_layer(gd_us, "state"),
      by = c("statefp", "division", "region", "stusps", "name")
    ) |>
    sf::st_as_sf()

  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = "statefp")
  state <- snake_case_geolevel(state)

  expect_equal(
    state$snake_case,
    TRUE
  )
  expect_equal(
    names(state$data),
    c("statefp", "division", "region", "stusps", "name", "intptlon",
      "intptlat")
  )
  expect_equal(names(state$geometry$polygon),
               c("statefp", "geom"))
})


test_that("add_geometry()", {
  layer_us_state <-
    dplyr::inner_join(
      get_level_data_geo(gd_us, "state"),
      get_level_layer(gd_us, "state"),
      by = c("statefp", "division", "region", "stusps", "name")
    ) |>
    sf::st_as_sf()

  us_state_point <-
    coordinates_to_geometry(layer_us_state,
                            lon_lat = c("intptlon", "intptlat"))
  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = "statefp",
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
    c("statefp", "division", "region", "stusps", "name", "intptlon",
      "intptlat")
  )
  expect_equal(names(state$geometry$polygon),
               c("statefp", "geom"))
  expect_equal(names(state$geometry$point),
               c("statefp", "geometry"))
  expect_equal(nrow(state$data),
               52)
  expect_equal(nrow(state$geometry$polygon),
               52)
  expect_equal(nrow(state$geometry$point),
               52)

})



test_that("add_geometry() and get_empty_geometry_instances()", {
  layer_us_state <-
    dplyr::inner_join(
      get_level_data_geo(gd_us, "state"),
      get_level_layer(gd_us, "state"),
      by = c("statefp", "division", "region", "stusps", "name")
    ) |>
    sf::st_as_sf()

  us_state_point <-
    coordinates_to_geometry(layer_us_state,
                            lon_lat = c("intptlon", "intptlat"))
  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = "statefp",
             snake_case = TRUE) |>
    add_geometry(layer = us_state_point)

  res <- state |> get_empty_geometry_instances(geometry = "point")

  expect_equal(
    names(state),
    c("name", "key", "snake_case", "data", "geometry")
  )
  expect_equal(
    names(state$data),
    c("statefp", "division", "region", "stusps", "name", "intptlon",
      "intptlat")
  )
  expect_equal(names(state$geometry$polygon),
               c("statefp", "geom"))
  expect_equal(names(state$geometry$point),
               c("statefp", "geometry"))
  expect_equal(nrow(state$data),
               52)
  expect_equal(nrow(state$geometry$polygon),
               52)
  expect_equal(nrow(state$geometry$point),
               52)

  expect_equal(names(res),
               c("statefp", "division", "region", "stusps", "name", "intptlon",
                 "intptlat"))

  expect_equal(nrow(res),
               0)
})

test_that("complete_point_geometry()", {
  layer_us_state <-
    dplyr::inner_join(
      get_level_data_geo(gd_us, "state"),
      get_level_layer(gd_us, "state"),
      by = c("statefp", "division", "region", "stusps", "name")
    ) |>
    sf::st_as_sf()

  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = "statefp",
             snake_case = TRUE) |>
    complete_point_geometry()

  expect_equal(names(state$geometry$polygon),
               c("statefp", "geom"))

  expect_equal(names(state$geometry$point),
               c("statefp", "geom"))
})

test_that("complete_point_geometry()", {
  layer_us_state <-
    dplyr::inner_join(
      get_level_data_geo(gd_us, "state"),
      get_level_layer(gd_us, "state"),
      by = c("statefp", "division", "region", "stusps", "name")
    ) |>
    sf::st_as_sf()

  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = "statefp",
             snake_case = TRUE) |>
    complete_point_geometry()
  state <- state |>
    complete_point_geometry()

  expect_equal(names(state$geometry$polygon),
               c("statefp", "geom"))

  expect_equal(names(state$geometry$point),
               c("statefp", "geom"))
})

test_that("complete_point_geometry()", {
  layer_us_state <-
    dplyr::inner_join(
      get_level_data_geo(gd_us, "state"),
      get_level_layer(gd_us, "state"),
      by = c("statefp", "division", "region", "stusps", "name")
    ) |>
    sf::st_as_sf()

  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = "statefp",
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
  layer_us_state <-
    dplyr::inner_join(
      get_level_data_geo(gd_us, "state"),
      get_level_layer(gd_us, "state"),
      by = c("statefp", "division", "region", "stusps", "name")
    ) |>
    sf::st_as_sf()

  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = "statefp",
             snake_case = TRUE) |>
    complete_point_geometry()

  state_1 <- state |>
    get_level_layer("polygon")

  state_2 <- state |>
    get_level_layer("point", only_key = TRUE)

  expect_equal(class(state_1),
               c("sf", "tbl_df", "tbl", "data.frame"))

  expect_equal(class(state_2),
               c("sf", "tbl_df", "tbl", "data.frame"))

  expect_equal(names(state_1),
               c("statefp", "division", "region", "stusps", "name", "intptlon",
                 "intptlat", "geom"))

  expect_equal(names(state_2),
               c("statefp", "geom"))

  expect_equal(nrow(state_1),
               52)

  expect_equal(nrow(state_2),
               52)

})
