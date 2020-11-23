context("test check_key")

library(sf) # It has to be included even if it is not used directly.

test_that("check_key works", {
  us_region <-
    get_level_layer(gd_us_city, level_name = "region", attributes = TRUE)

  is_key <- check_key(us_region, key = c("name"))
  expect_equal(is_key,
               TRUE)

  is_key <- check_key(us_region, key = c("lsad"))
  expect_equal(is_key,
               FALSE)
})
