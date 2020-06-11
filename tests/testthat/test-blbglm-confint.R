future::plan(future::sequential)

mtcars <- read.csv("data/mtcars.csv")
iris <- read.csv("data/iris_subset.csv")

test_that("linear regression confints", {
  b <- blbglm(mpg ~ wt, data = mtcars, B = 100, even_split = TRUE, use_plan = FALSE)
  sigma_ci <- sigma(b, confidence = TRUE)
  expect_equal(names(sigma_ci), c("sigma", "lwr", "upr"))
  expect_gt(sigma_ci["sigma"], sigma_ci["lwr"])
  expect_lt(sigma_ci["sigma"], sigma_ci["upr"])
  wt_ci <- confint(b, "wt")
  expect_lt(wt_ci[1], wt_ci[2])
})

test_that("logistic regression confints", {
  expect_warning(b <- blbglm(Species ~ Sepal.Length, family = binomial, data = iris, B = 100, even_split = TRUE, use_plan = FALSE))
  sigma_ci <- sigma(b, confidence = TRUE)
  expect_equal(names(sigma_ci), c("sigma", "lwr", "upr"))
  expect_gt(sigma_ci["sigma"], sigma_ci["lwr"])
  expect_lt(sigma_ci["sigma"], sigma_ci["upr"])
  sepal_ci <- confint(b, "Sepal.Length")
  expect_lt(sepal_ci[1], sepal_ci[2])
})
