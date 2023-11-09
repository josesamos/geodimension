context("test get_level_layer")

library(sf) # It has to be included even if it is not used directly.

test_that("get_level_layer works", {
  ll <- gd_us |>
    get_level_layer(level_name = "division",
                    only_key = TRUE,
                    surrogate_key = TRUE)

  expect_equal(
    attributes(ll),
    list(
      names = c("division_key", "geoid", "geom"),
      row.names = 1:9,
      class = c("sf", "tbl_df", "tbl", "data.frame"),
      sf_column = "geom",
      agr = structure(
        c(division_key = NA_integer_, geoid = NA_integer_),
        class = "factor",
        .Label = c("constant", "aggregate",
                   "identity")
      )
    )
  )

  ll <- gd_us |>
    get_level_layer(level_name = "division",
                    surrogate_key = TRUE,
                    inherited = TRUE)

  expect_equal(
    attributes(ll),
    list(
      names = c(
        "division_key",
        "geoid",
        "divisionce",
        "affgeoid",
        "name",
        "lsad",
        "aland",
        "awater",
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
        "REGION_awater",
        "geom"
      ),
      row.names = 1:9,
      class = c("sf",
                "tbl_df", "tbl", "data.frame"),
      sf_column = "geom",
      agr = structure(
        c(
          division_key = NA_integer_,
          geoid = NA_integer_,
          divisionce = NA_integer_,
          affgeoid = NA_integer_,
          name = NA_integer_,
          lsad = NA_integer_,
          aland = NA_integer_,
          awater = NA_integer_,
          NATION_nation_key = NA_integer_,
          NATION_geoid = NA_integer_,
          NATION_affgeoid = NA_integer_,
          NATION_name = NA_integer_,
          REGION_region_key = NA_integer_,
          REGION_geoid = NA_integer_,
          REGION_regionce = NA_integer_,
          REGION_affgeoid = NA_integer_,
          REGION_name = NA_integer_,
          REGION_lsad = NA_integer_,
          REGION_aland = NA_integer_,
          REGION_awater = NA_integer_
        ),
        class = "factor",
        .Label = c("constant",
                   "aggregate", "identity")
      )
    )
  )

})
