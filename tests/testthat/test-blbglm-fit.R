suppressWarnings(future::plan(future::multiprocess))

mtcars <- read.csv("data/mtcars.csv")
iris <- read.csv("data/iris_subset.csv")

test_that("parallelized, data given directly", {
  b <- blbglm(mpg ~ wt, data = mtcars, B = 100)
  expect_s3_class(b, "blbglm")
  co <- coef(b)
  expect_equal(length(co), 2)
  expect_type(sigma(b), "double")
})

test_that("parallelized, data read from files", {
  filepaths <- c("data/mtcars1.csv", "data/mtcars2.csv")
  b <- blbglm(mpg ~ wt, filepaths = filepaths, m = 2, B = 100)
  expect_s3_class(b, "blbglm")
  co <- coef(b)
  expect_equal(length(co), 2)
  expect_type(sigma(b), "double")
})

future::plan(future::sequential)

test_that("sequential, data given directly", {
  expect_warning(b <- blbglm(mpg ~ wt, data = mtcars, B = 100))
  expect_s3_class(b, "blbglm")
  co <- coef(b)
  expect_equal(length(co), 2)
  expect_type(sigma(b), "double")
})

test_that("sequential, data read from files", {
  filepaths <- c("data/mtcars1.csv", "data/mtcars2.csv")
  expect_warning(b <- blbglm(mpg ~ wt, filepaths = filepaths, B = 100))
  expect_s3_class(b, "blbglm")
  co <- coef(b)
  expect_equal(length(co), 2)
  expect_type(sigma(b), "double")
})

test_that("logistic regression works", {
  expect_warning(b <- blbglm(Species ~ Sepal.Length, family = binomial, data = iris, B = 100, use_plan = FALSE))
  expect_s3_class(b, "blbglm")
  co <- coef(b)
  expect_equal(length(co), 2)
  expect_type(sigma(b), "double")
})

test_that("multivariate formula works", {
  expect_warning(b <- blbglm(Species ~ Sepal.Length + Sepal.Width, family = binomial, data = iris, B = 100, use_plan = FALSE))
  expect_s3_class(b, "blbglm")
  co <- coef(b)
  expect_equal(length(co), 3)
  expect_type(sigma(b), "double")
})

test_that("dot formula works", {
  expect_warning(b <- blbglm(Species ~ ., family = binomial, data = iris, B = 100, use_plan = FALSE))
  expect_s3_class(b, "blbglm")
  co <- coef(b)
  expect_equal(length(co), 3)
  expect_type(sigma(b), "double")
})
