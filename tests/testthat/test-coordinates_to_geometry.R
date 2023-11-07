test_that("coordinates_to_geometry()", {
  us_state_point <-
    coordinates_to_geometry(layer_us_state,
                            lon_lat = c("intPtlon", "intptlat"))
  expect_equal(
    attributes(us_state_point),
    list(
      names = c(
        "region",
        "division",
        "statefp",
        "statens",
        "geoid",
        "stusps",
        "name",
        "lsad",
        "mtfcc",
        "funcstat",
        "aland",
        "awater",
        "shape_length",
        "shape_area",
        "geoid_data",
        "geometry"
      ),
      row.names = 1:52,
      sf_column = "geometry",
      agr = structure(
        c(
          region = NA_integer_,
          division = NA_integer_,
          statefp = NA_integer_,
          statens = NA_integer_,
          geoid = NA_integer_,
          stusps = NA_integer_,
          name = NA_integer_,
          lsad = NA_integer_,
          mtfcc = NA_integer_,
          funcstat = NA_integer_,
          aland = NA_integer_,
          awater = NA_integer_,
          shape_length = NA_integer_,
          shape_area = NA_integer_,
          geoid_data = NA_integer_
        ),
        class = "factor",
        .Label = c("constant",
                   "aggregate", "identity")
      ),
      class = c("sf", "tbl_df", "tbl",
                "data.frame")
    )
  )
})
