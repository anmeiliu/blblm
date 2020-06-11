future::plan(future::sequential)

mtcars <- read.csv("data/mtcars.csv")
iris <- read.csv("data/iris_subset.csv")

test_that("respects use_plan = FALSE", {
  filepaths = c("data/mtcars1.csv", "data/mtcars2.csv")
  b <- blbglm(mpg ~ wt, filepaths = filepaths, m = 2, B = 100, use_plan = FALSE)
  expect_s3_class(b, "blbglm")
})

test_that("respects file split over m", {
  filepaths = c("data/mtcars1.csv", "data/mtcars2.csv")
  expect_warning(b <- blbglm(mpg ~ wt, filepaths = filepaths, B = 100, use_plan = FALSE))
  expect_s3_class(b, "blbglm")
})

test_that("respects min_subsample_size and even_split", {
  # produces NaN due to subsamples being too small to calculate sigma
  b <- blbglm(mpg ~ wt, data = mtcars, m = 20, B = 100, min_subsample_size = 1, use_plan = FALSE)
  expect_equal(sigma(b), NaN)
  # all of these results should be able to calculate non-NaN sigma
  b <- blbglm(mpg ~ wt, data = mtcars, m = 10, B = 100, min_subsample_size = 3, use_plan = FALSE)
  expect_gt(sigma(b), 0)
  b <- blbglm(mpg ~ wt, data = mtcars, m = 10, B = 100, even_split = TRUE, use_plan = FALSE)
  expect_gt(sigma(b), 0)
  expect_warning(b <- blbglm(mpg ~ wt, data = mtcars, m = 10, B = 100, min_subsample_size = 3, even_split = TRUE, use_plan = FALSE))
  expect_gt(sigma(b), 0)
  expect_error(b <- blbglm(mpg ~ wt, data = mtcars, m = 10, B = 100, min_subsample_size = 4, even_split = TRUE, use_plan = FALSE))
})