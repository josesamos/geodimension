context("test get_empty_geometry_instances")

test_that("get_empty_geometry_instances works", {
  us_state_point <-
    coordinates_to_geometry(layer_us_state,
                            lon_lat = c("intptlon", "intptlat"))

  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = c("geoid")) |>
    add_geometry(layer = us_state_point)

  res <- state |> get_empty_geometry_instances(geometry = "point")

  expect_equal(res,
               structure(
                 list(
                   state_key = integer(0),
                   geoid = character(0),
                   region = character(0),
                   division = character(0),
                   statefp = character(0),
                   statens = character(0),
                   stusps = character(0),
                   name = character(0),
                   lsad = character(0),
                   mtfcc = character(0),
                   funcstat = character(0),
                   aland = numeric(0),
                   awater = numeric(0),
                   intptlat = character(0),
                   intptlon = character(0),
                   shape_length = numeric(0),
                   shape_area = numeric(0),
                   geoid_data = character(0)
                 ),
                 row.names = integer(0),
                 class = c("tbl_df",
                           "tbl", "data.frame")
               ))
})
