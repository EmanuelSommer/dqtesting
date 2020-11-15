test_that("lof_fun", {
  expect_equal(ggplot2::is.ggplot(
    lof_fun(dummy_data)$boxplot
  ), TRUE)
  expect_equal(is.numeric(
    lof_fun(dummy_data)$scores
  ), TRUE)
  expect_equal(all(lof_fun(dummy_data)$scores >= 0), TRUE)
})



test_that("extract_rows_score", {
  expect_equal(
    extract_rows_score(dummy_data, lof_fun(dummy_data))$LOF_scores,
    sort(extract_rows_score(dummy_data, lof_fun(dummy_data))$LOF_scores, decreasing = TRUE)
  )
  expect_equal(
    dim(extract_rows_score(dummy_data, lof_fun(dummy_data)))[2],
    dim(dummy_data)[2] + 1
  )
})

test_that("lof_vis", {
  expect_error(ggplot2::is.ggplot(
    lof_vis(dummy_data, lof_fun(dummy_data))
  ))
  expect_equal(ggplot2::is.ggplot(
    lof_vis(dplyr::select(dummy_data, "num1", "num2"), lof_fun(dplyr::select(dummy_data, "num1", "num2")))
  ), TRUE)
  expect_equal(ggplot2::is.ggplot(
    lof_vis(dplyr::select(dummy_data, "date1", "num2"), lof_fun(dplyr::select(dummy_data, "date1", "num2")))
  ), TRUE)
  expect_equal(ggplot2::is.ggplot(
    lof_vis(dplyr::select(dummy_data, "datetime1", "num3"), lof_fun(dplyr::select(dummy_data, "datetime1", "num3")))
  ), TRUE)
  expect_equal(ggplot2::is.ggplot(
    lof_vis(dplyr::select(dummy_data, "num1", "fact1"), lof_fun(dplyr::select(dummy_data, "num1", "fact1")))
  ), TRUE)
  expect_equal(ggplot2::is.ggplot(
    lof_vis(dplyr::select(dummy_data, "char1", "log1"), lof_fun(dplyr::select(dummy_data, "char1", "log1")))
  ), TRUE)
})
