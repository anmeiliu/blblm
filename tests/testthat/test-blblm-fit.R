test_that("sequential, data given directly", {
  future::plan(future::sequential)
  mtcars <- read.csv("data/mtcars.csv")
  b <- blblm(mpg ~ wt, mtcars, B = 100)
  expect_s3_class(b, "blblm")
  co <- coef(b)
  expect_equal(length(co), 2)
  expect_type(sigma(b), "double")
})

test_that("parallelized, data given directly", {
  suppressWarnings(future::plan(future::multiprocess))
  mtcars <- read.csv("data/mtcars.csv")
  b <- blblm(mpg ~ wt, mtcars, B = 100)
  expect_s3_class(b, "blblm")
  co <- coef(b)
  expect_equal(length(co), 2)
  expect_type(sigma(b), "double")
})

test_that("sequential, data read from files", {
  future::plan(future::sequential)
  filepaths = c("data/mtcars1.csv", "data/mtcars2.csv")
  b <- blblm(mpg ~ wt, filepaths = filepaths, B = 100)
  expect_s3_class(b, "blblm")
  co <- coef(b)
  expect_equal(length(co), 2)
  expect_type(sigma(b), "double")
})

test_that("parallelized, data read from files", {
  suppressWarnings(future::plan(future::multiprocess))
  filepaths = c("data/mtcars1.csv", "data/mtcars2.csv")
  b <- blblm(mpg ~ wt, filepaths = filepaths, B = 100)
  expect_s3_class(b, "blblm")
  co <- coef(b)
  expect_equal(length(co), 2)
  expect_type(sigma(b), "double")
})