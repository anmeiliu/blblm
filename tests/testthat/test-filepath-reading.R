test_that("read_data works", {
  data <- read_data("csv_data.csv", read.csv)
  expect_equal(names(data), c("x", "y"))
  expect_equal(data$x, c(1.0, 3.1, 5.0))
  expect_equal(data$y, c(2.0, 4.1, 6.0))
})

test_that("read_data respects optional arguments", {
  data <- read_data("csv_data_optional.csv", read.csv, header = FALSE)
  expect_equal(names(data), c("V1", "V2"))
  expect_equal(data$V1, c(1.0, 3.1, 5.0))
  expect_equal(data$V2, c(2.0, 4.1, 6.0))
})

test_that("read_data works with readr", {
  data <- read_data("csv_data.csv", readr::read_csv, col_types = "dd")
  expect_equal(names(data), c("x", "y"))
  expect_equal(data$x, c(1.0, 3.1, 5.0))
  expect_equal(data$y, c(2.0, 4.1, 6.0))
})