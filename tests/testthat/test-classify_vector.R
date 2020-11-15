test_that("error for false input of cat_thres", {
  expect_error(classify_vector(c(1:100, 0.1), cat_thres = -1))
  expect_error(classify_vector(c(1:100, 0.1), cat_thres = "abc"))
  expect_error(classify_vector(as.integer(1:100), cat_thres = -1))
  expect_error(classify_vector(as.integer(1, 2), cat_thres = "abc"))
})

test_that("numeric continuous works", {
  expect_equal(classify_vector(c(1:100, 0.1, 0.2)), "cont")
  expect_equal(classify_vector(c(1:100, NA, 0.2)), "cont")
  expect_equal(classify_vector(0.3, cat_thres = 0), "cont")
})

test_that("integer continuous works", {
  expect_equal(classify_vector(as.integer(1:100)), "cont")
  expect_equal(classify_vector(as.integer(1:100, NA)), "cont")
  expect_equal(classify_vector(as.integer(1, 2), cat_thres = 0), "cont")
})

test_that("numeric categorical works", {
  expect_equal(classify_vector(c(1:100, 0.1, 0.2), cat_thres = 102), "cat")
  expect_equal(classify_vector(c(1:100, NA, 0.2), cat_thres = 102), "cat")
  expect_equal(classify_vector(0.3, cat_thres = 1), "cat")
  expect_equal(classify_vector(rep(c(1:20, 0.1), 100)), "cat")
})

test_that("integer categorical works", {
  expect_equal(classify_vector(as.integer(1:100), cat_thres = 102), "cat")
  expect_equal(classify_vector(as.integer(1:100, NA), cat_thres = 102), "cat")
  expect_equal(classify_vector(as.integer(1, 2), cat_thres = 2), "cat")
  expect_equal(classify_vector(as.integer(rep(1:20, 100))), "cat")
})

test_that("character categorical works", {
  expect_equal(classify_vector(sample(c("abc", "def", "ghi"), 100, replace = TRUE), cat_thres = 102), "cat")
  expect_equal(classify_vector(sample(c("abc", "def", "ghi"), 100, replace = TRUE), cat_thres = 10), "cat")
  expect_equal(classify_vector(sample(c("abc", "def", "ghi", NA), 100, replace = TRUE)), "cat")
  expect_equal(classify_vector("abc"), "cat")
})

test_that("logical categorical works", {
  expect_equal(classify_vector(sample(c(TRUE, FALSE, FALSE), 100, replace = TRUE), cat_thres = 102), "cat")
  expect_equal(classify_vector(sample(c(TRUE, FALSE, FALSE), 100, replace = TRUE), cat_thres = 10), "cat")
  expect_equal(classify_vector(sample(c(TRUE, FALSE, FALSE, NA), 100, replace = TRUE)), "cat")
  expect_equal(classify_vector(TRUE), "cat")
})

test_that("factor categorical works", {
  expect_equal(classify_vector(sample(as.factor(c(TRUE, FALSE, FALSE)), 100, replace = TRUE), cat_thres = 102), "cat")
  expect_equal(classify_vector(sample(as.factor(1:10), 100, replace = TRUE), cat_thres = 10), "cat")
  expect_equal(classify_vector(as.factor(c("abc", NA))), "cat")
  expect_equal(classify_vector(as.factor("Hello World!")), "cat")
})

test_that("POSIXct/ POSIXlt datetime works", {
  expect_equal(classify_vector(sample(as.POSIXct(c(
    "2000-12-12 00:12:33",
    "3000-01-01 11:11:23",
    "0110-02-22 00:00:01",
    NA
  )), 100, replace = TRUE), cat_thres = 102), "datetime")
  expect_equal(classify_vector(sample(as.POSIXct(c(
    "2000-12-12 00:12:33",
    "3000-01-01 11:11:23",
    "0110-02-22 00:00:01",
    NA
  )), 100, replace = TRUE), cat_thres = 10), "datetime")
  expect_equal(classify_vector(sample(as.POSIXlt(c(
    "2000-12-12 00:12:33",
    "3000-01-01 11:11:23",
    "0110-02-22 00:00:01",
    NA
  )), 100, replace = TRUE)), "datetime")
  expect_equal(classify_vector(as.POSIXlt("2000-12-12 00:12:33")), "datetime")
  expect_equal(classify_vector(as.POSIXlt(NA)), "datetime")
})


test_that("Dates work", {
  expect_equal(classify_vector(sample(as.Date(c(
    "2000-12-12",
    "3000-01-01",
    "0110-02-22",
    NA
  )), 100, replace = TRUE), cat_thres = 102), "date")
  expect_equal(classify_vector(sample(as.Date(c(
    "2000-12-12",
    "3000-01-01",
    "0110-02-22",
    NA
  )), 100, replace = TRUE), cat_thres = 10), "date")
  expect_equal(classify_vector(sample(as.Date(c(
    "2000-12-12",
    "3000-01-01",
    "0110-02-22",
    NA
  )), 100, replace = TRUE)), "date")
  expect_equal(classify_vector(as.Date("2000-12-12")), "date")
  expect_equal(classify_vector(as.Date(NA)), "date")
})
