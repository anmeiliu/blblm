future::plan(future::sequential)

mtcars <- read.csv("data/mtcars.csv")
iris <- read.csv("data/iris_subset.csv")

test_that("linear regression confints", {
  b <- blbglm(mpg ~ wt, data = mtcars, B = 100, even_split = TRUE, use_plan = FALSE)
  pred <- predict(b, mtcars)
  expect_type(pred, "double")
  expect_true(all(!is.na(pred)))
  pred_ci <- predict(b, mtcars, confidence = TRUE)
  expect_equal(dim(pred_ci), c(32, 3))
  expect_true(all(pred_ci[, "fit"] > pred_ci[, "lwr"]))
  expect_true(all(pred_ci[, "fit"] < pred_ci[, "upr"]))
})

test_that("logistic regression confints", {
  expect_warning(b <- blbglm(Species ~ Sepal.Length, family = binomial, data = iris, B = 100, even_split = TRUE, use_plan = FALSE))
  pred <- predict(b, iris)
  expect_type(pred, "double")
  expect_true(all(!is.na(pred)))
  pred_ci <- predict(b, iris, confidence = TRUE)
  expect_equal(dim(pred_ci), c(100, 3))
  expect_true(all(pred_ci[, "fit"] > pred_ci[, "lwr"]))
  expect_true(all(pred_ci[, "fit"] < pred_ci[, "upr"]))
})
