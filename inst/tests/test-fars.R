
library(MyFirstPackage)
library(testthat)

context("FARS tests")

test_that("Testing make_filename", {
  expect_match(make_filename(2013), "accident_2013.csv.bz2")
  expect_warning(fars_read_years(2011), "invalid year: 2011")
  expect_error(fars_map_state(1,2011), "file 'accident_2011.csv.bz2' does not exist")
})


