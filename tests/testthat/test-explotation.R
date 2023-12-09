test_that("get_level_geometries()", {
  expect_equal(gd_us |>
                 get_level_geometries(level_name = "state"),
               c("point", "polygon"))

})

test_that("get_level_names()", {
  expect_equal(
    gd_us |>
      get_level_names(),
    c("country", "county", "division", "place", "region", "state")
  )

})

test_that("select_levels()", {
  gd_us_2 <- gd_us |>
    select_levels(level_names = c("state", "county", "place", "region"))

  expect_equal(names(gd_us_2$geolevel),
               c("place", "county", "state", "region"))

  expect_equal(gd_us_2$relation,
               list(
                 place = list(county = list(
                   lower_fk = "county_geoid", upper_pk = "geoid"
                 )),
                 county = list(state = list(
                   lower_fk = "statefp", upper_pk = "statefp"
                 ))
               ))

})



test_that("get_level_data()", {
  ld_1 <- gd_us |>
    get_level_data(level_name = "county")

  ld_2 <- gd_us |>
    get_level_data(level_name = "county",
                   inherited = TRUE)

  expect_equal(nrow(ld_1),
               nrow(ld_2))

  expect_equal(names(ld_1),
               c("geoid", "statefp", "name", "type"))

  expect_equal(
    names(ld_2),
    c(
      "geoid",
      "statefp",
      "name",
      "type",
      "state_division",
      "state_region",
      "state_stusps",
      "state_name",
      "state_division_country",
      "state_division_region_code",
      "state_division_division_name",
      "state_division_region_country",
      "state_division_region_region_name"
    )
  )

})


test_that("get_level_layer()", {
  ll_1 <- gd_us |>
    get_level_layer(level_name = "county",
                    geometry = "polygon")

  ll_2 <- gd_us |>
    get_level_layer(level_name = "county",
                    geometry = "polygon",
                    inherited = TRUE)

  expect_equal(nrow(ll_1),
               nrow(ll_2))

  expect_equal(names(ll_1),
               c("geoid", "statefp", "name", "type", "geom"))

  expect_equal(
    names(ll_2),
    c(
      "geoid",
      "statefp",
      "name",
      "type",
      "state_division",
      "state_region",
      "state_stusps",
      "state_name",
      "state_division_country",
      "state_division_region_code",
      "state_division_division_name",
      "state_division_region_country",
      "state_division_region_region_name",
      "geom"
    )
  )

})
