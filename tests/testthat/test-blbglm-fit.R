test_that("sequential, data given directly", {
  future::plan(future::sequential)
  mtcars <- read.csv("data/mtcars.csv")
  expect_warning(b <- blbglm(mpg ~ wt, data = mtcars, B = 100))
  expect_s3_class(b, "blbglm")
  co <- coef(b)
  expect_equal(length(co), 2)
  expect_type(sigma(b), "double")
})

test_that("parallelized, data given directly", {
  suppressWarnings(future::plan(future::multiprocess))
  mtcars <- read.csv("data/mtcars.csv")
  b <- blbglm(mpg ~ wt, data = mtcars, B = 100)
  expect_s3_class(b, "blbglm")
  co <- coef(b)
  expect_equal(length(co), 2)
  expect_type(sigma(b), "double")
})

test_that("sequential, data read from files", {
  future::plan(future::sequential)
  filepaths = c("data/mtcars1.csv", "data/mtcars2.csv")
  expect_warning(b <- blbglm(mpg ~ wt, filepaths = filepaths, B = 100))
  expect_s3_class(b, "blbglm")
  co <- coef(b)
  expect_equal(length(co), 2)
  expect_type(sigma(b), "double")
})

test_that("parallelized, data read from files", {
  suppressWarnings(future::plan(future::multiprocess))
  filepaths = c("data/mtcars1.csv", "data/mtcars2.csv")
  b <- blbglm(mpg ~ wt, filepaths = filepaths, m = 2, B = 100)
  expect_s3_class(b, "blbglm")
  co <- coef(b)
  expect_equal(length(co), 2)
  expect_type(sigma(b), "double")
})

test_that("respects use_plan = FALSE", {
  future::plan(future::sequential)
  filepaths = c("data/mtcars1.csv", "data/mtcars2.csv")
  b <- blbglm(mpg ~ wt, filepaths = filepaths, m = 2, B = 100, use_plan = FALSE)
  expect_s3_class(b, "blbglm")
})

test_that("respects file split over m", {
  future::plan(future::sequential)
  filepaths = c("data/mtcars1.csv", "data/mtcars2.csv")
  expect_warning(b <- blbglm(mpg ~ wt, filepaths = filepaths, B = 100, use_plan = FALSE))
  expect_s3_class(b, "blbglm")
})

test_that("logistic regression works", {
  future::plan(future::sequential)
  iris <- read.csv("data/iris_subset.csv")
  expect_warning(b <- blbglm(Species ~ Sepal.Length + Sepal.Width, family = binomial, data = iris, B = 100, use_plan = FALSE))
  expect_s3_class(b, "blbglm")
  co <- coef(b)
  expect_equal(length(co), 3)
  expect_type(sigma(b), "double")
})