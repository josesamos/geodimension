context("test get_level_data")

test_that("get_level_data works", {
  ld <- gd_us |>
    get_level_data(level_name = "state",
                   inherited = TRUE)

  expect_equal(attributes(ld),
               list(
                 names = c(
                   "state_key",
                   "geoid",
                   "region",
                   "division",
                   "statefp",
                   "statens",
                   "stusps",
                   "name",
                   "lsad",
                   "mtfcc",
                   "funcstat",
                   "aland",
                   "awater",
                   "intptlat",
                   "intptlon",
                   "shape_length",
                   "shape_area",
                   "geoid_data",
                   "DIVISION_division_key",
                   "DIVISION_geoid",
                   "DIVISION_divisionce",
                   "DIVISION_affgeoid",
                   "DIVISION_name",
                   "DIVISION_lsad",
                   "DIVISION_aland",
                   "DIVISION_awater",
                   "NATION_nation_key",
                   "NATION_geoid",
                   "NATION_affgeoid",
                   "NATION_name",
                   "REGION_region_key",
                   "REGION_geoid",
                   "REGION_regionce",
                   "REGION_affgeoid",
                   "REGION_name",
                   "REGION_lsad",
                   "REGION_aland",
                   "REGION_awater"
                 ),
                 row.names = 1:52,
                 class = c("tbl_df", "tbl",
                           "data.frame")
               ))
})
