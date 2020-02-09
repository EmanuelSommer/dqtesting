##############################################################################################################
#
#                        General tests
#
##############################################################################################################

test_that("all missing values", {
  expect_equal(perform_dqtest(x = rep(NA,20))$pre_scheduled_return , "Yes, all NA.")
  expect_equal(perform_dqtest(x = as.character(rep(NA,20)))$pre_scheduled_return , "Yes, all NA.")
  expect_equal(perform_dqtest(x = NA)$pre_scheduled_return , "Yes, all NA.")
  expect_equal(perform_dqtest(x = rep(NA,20), exclude_values = 1:10)$pre_scheduled_return , "Yes, all NA.")
  expect_equal(perform_dqtest(x = rep(NA,20), exclude_values = "test")$pre_scheduled_return , "Yes, all NA.")
})

test_that("classification", {
  expect_equal(perform_dqtest(x = c(1:100,0.7))$classification , "cont")
  expect_equal(perform_dqtest(x = c(1:10,0.7), cat_thres = 3)$classification , "cont")
  expect_equal(perform_dqtest(x = "abc")$classification , "cat")
  expect_equal(perform_dqtest(x = as.Date(c("2000-12-12",
                                            "3000-01-01",
                                            "0110-02-22",
                                            NA)))$classification , "date")
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-12-12 00:12:33",
                                               "3000-01-01 11:11:23",
                                               "0110-02-22 00:00:01",
                                               NA)))$classification , "datetime")
})

test_that("initial length", {
  expect_equal(perform_dqtest(x = c(1:10,NA))$initial_length , 11)
  expect_equal(perform_dqtest(x = "a")$initial_length , 1)
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-12-12 00:12:33",
                                               "3000-01-01 11:11:23",
                                               "0110-02-22 00:00:01",
                                               NA)))$initial_length , 4)
})

test_that("NA counts/ relative", {
  expect_equal(perform_dqtest(x = c(1:10,NA))$abs_na , 1)
  expect_equal(perform_dqtest(x = c("abc","cdf",rep(NA,10)))$abs_na , 10)
  expect_equal(perform_dqtest(x = c(1:9,NA))$rel_na , 0.1)
  expect_equal(perform_dqtest(x = c("abc","def","xyz",NA))$rel_na , 0.25)
})


##############################################################################################################
#
#                        Exclusions tests
#
##############################################################################################################

test_that("Exclusions continuous", {
  expect_equal(perform_dqtest(x = c(1:100,NA))$exclusions , "No exclusions.")
  expect_equal(perform_dqtest(x = c(1:100,NA), exclude_smaller_than = 0)$exclusions , "No exclusions.")
  expect_equal(perform_dqtest(x = c(1:100,NA), exclude_greater_than = 100)$exclusions , "No exclusions.")
  expect_equal(perform_dqtest(x = c(1:100,NA),
                              exclude_smaller_than = 0,
                              exclude_greater_than = 100)$exclusions , "No exclusions.")
  expect_equal(perform_dqtest(x = c(1:100,NA), exclude_values = c(1000,0.4))$exclusions , "No exclusions.")
  expect_equal(perform_dqtest(x = c(1:100,NA),
                              exclude_smaller_than = 1,
                              exclude_greater_than = 100,
                              exclude_values = c(1000,0.4))$exclusions , "No exclusions.")
  expect_error(perform_dqtest(x = c(1:100,NA), exclude_values = NA)$exclusions)
  expect_error(perform_dqtest(x = c(1:100,NA), exclude_greater_than = NA)$exclusions)
  expect_error(perform_dqtest(x = c(1:100,NA), exclude_smaller_than = NA)$exclusions)
  expect_equal(perform_dqtest(x = c(1:100), exclude_smaller_than = 1.1)$exclusions$abs_excluded , 1)
  expect_equal(perform_dqtest(x = c(1:100), exclude_smaller_than = 1.1,
                              exclude_greater_than = 98)$exclusions$abs_excluded , 3)
  expect_equal(perform_dqtest(x = c(1:100), exclude_smaller_than = 1.1)$exclusions$rel_excluded , 0.01)
  expect_equal(perform_dqtest(x = c(1:100), exclude_greater_than = 91,
                              exclude_values = 9)$exclusions$rel_excluded , 0.1)
  expect_equal(perform_dqtest(x = c(1:100), exclude_smaller_than = 1000)$exclusions$rel_excluded , 1)
  expect_equal(perform_dqtest(x = c(1:100), exclude_smaller_than = 1000)$exclusions$abs_excluded , 100)
  expect_equal(perform_dqtest(x = c(1:100), exclude_smaller_than = 1000)$pre_scheduled_return , "Yes, all not missing excluded.")
  expect_equal(perform_dqtest(x = c(1:100),
                              exclude_smaller_than = 10,
                              exclude_values = 9:33,
                              exclude_greater_than = 30)$exclusions$rel_excluded , 1)
  expect_equal(perform_dqtest(x = c(1:100),
                              exclude_smaller_than = 10,
                              exclude_values = 9:33,
                              exclude_greater_than = 30)$exclusions$abs_excluded , 100)
  expect_equal(perform_dqtest(x = c(1:100),
                              exclude_smaller_than = 10,
                              exclude_values = 9:33,
                              exclude_greater_than = 30)$pre_scheduled_return , "Yes, all not missing excluded.")
})


test_that("Exclusions categorical", {
  expect_equal(perform_dqtest(x = c("abc",rep("def",4),NA,"xyz",rep("test",3)))$exclusions , "No exclusions.")
  expect_equal(perform_dqtest(x = c("abc",rep("def",4),NA,"xyz",rep("test",3)),
                              exclude_values = c("a","b","c"))$exclusions , "No exclusions.")
  expect_error(perform_dqtest(x = c("abc",rep("def",4),NA,"xyz",rep("test",3)),
                              exclude_values = 1:10)$exclusions)
  expect_error(perform_dqtest(x = c("abc",rep("def",4),NA,"xyz",rep("test",3)),
                              exclude_values = NA)$exclusions)
  expect_equal(perform_dqtest(x = c("abc",rep("def",4),NA,"xyz",rep("test",3)),
                              exclude_values = c("abc"))$exclusions$rel_excluded , 0.1)
  expect_equal(perform_dqtest(x = c("abc",rep("def",4),NA,"xyz",rep("test",3)),
                              exclude_values = c("abc","test"))$exclusions$rel_excluded , 0.4)
  expect_equal(perform_dqtest(x = c("abc",rep("def",4),NA,"xyz",rep("test",3)),
                              exclude_values = c("abc","test","def","xyz"))$exclusions$rel_excluded , 0.9)
  expect_equal(perform_dqtest(x = c("abc",rep("def",4),NA,"xyz",rep("test",3)),
                              exclude_values = c("abc"))$exclusions$abs_excluded , 1)
  expect_equal(perform_dqtest(x = c("abc",rep("def",4),NA,"xyz",rep("test",3)),
                              exclude_values = c("abc","test"))$exclusions$abs_excluded , 4)
  expect_equal(perform_dqtest(x = c("abc",rep("def",4),NA,"xyz",rep("test",3)),
                              exclude_values = c("abc","test","def","xyz"))$exclusions$abs_excluded , 9)
  expect_equal(perform_dqtest(x = c("abc",rep("def",4),NA,"xyz",rep("test",3)),
                              exclude_values = c("abc","test","def","xyz"))$pre_scheduled_return , "Yes, all not missing excluded.")
})


test_that("Exclusions date", {
  expect_equal(perform_dqtest(x = as.Date(c("2000-12-12",
                                                   rep("3000-01-01",7),
                                                   "0110-02-22",
                                                   NA)))$exclusions , "No exclusions.")
  expect_equal(perform_dqtest(x = as.Date(c("2000-12-12",
                                            rep("3000-01-01",7),
                                            "0110-02-22",
                                            NA)),
                              exclude_values = as.Date("1999-01-01"))$exclusions , "No exclusions.")
  expect_equal(perform_dqtest(x = as.Date(c("2000-12-12",
                                            rep("3000-01-01",7),
                                            "0110-02-22",
                                            NA)),
                              exclude_smaller_than = as.Date("0011-01-01"))$exclusions , "No exclusions.")
  expect_equal(perform_dqtest(x = as.Date(c("2000-12-12",
                                            rep("3000-01-01",7),
                                            "0110-02-22",
                                            NA)),
                              exclude_greater_than = as.Date("4000-01-01"))$exclusions , "No exclusions.")
  expect_error(perform_dqtest(x = as.Date(c("2000-12-12",
                                            rep("3000-01-01",7),
                                            "0110-02-22",
                                            NA)),
                              exclude_greater_than = "4000-01-01")$exclusions)
  expect_error(perform_dqtest(x = as.Date(c("2000-12-12",
                                            rep("3000-01-01",7),
                                            "0110-02-22",
                                            NA)),
                              exclude_smaller_than = NA)$exclusions)
  expect_error(perform_dqtest(x = as.Date(c("2000-12-12",
                                            rep("3000-01-01",7),
                                            "0110-02-22",
                                            NA)),
                              exclude_values = "4000-01-01")$exclusions)
  expect_equal(perform_dqtest(x = as.Date(c("2000-12-12",
                                            rep("3000-01-01",7),
                                            "0110-02-22",
                                            NA)),
                              exclude_values = as.Date("2000-12-12"))$exclusions$abs_excluded , 1)
  expect_equal(perform_dqtest(x = as.Date(c("2000-12-12",
                                            rep("3000-01-01",7),
                                            "0110-02-22",
                                            NA)),
                              exclude_values = as.Date("3000-01-01"))$exclusions$abs_excluded , 7)
  expect_equal(perform_dqtest(x = as.Date(c("2000-12-12",
                                            rep("3000-01-01",7),
                                            "0110-02-22",
                                            NA)),
                              exclude_values = as.Date("2000-12-12"))$exclusions$rel_excluded , 0.1)
  expect_equal(perform_dqtest(x = as.Date(c("2000-12-12",
                                            rep("3000-01-01",7),
                                            "0110-02-22",
                                            NA)),
                              exclude_values = as.Date(c("3000-01-01","0110-02-22")))$exclusions$rel_excluded , 0.8)
  expect_equal(perform_dqtest(x = as.Date(c("2000-12-12",
                                            rep("3000-01-01",7),
                                            "0110-02-22",
                                            NA)),
                              exclude_greater_than = as.Date("2000-12-12"))$exclusions$rel_excluded , 0.7)
  expect_equal(perform_dqtest(x = as.Date(c("2000-12-12",
                                            rep("3000-01-01",7),
                                            "0110-02-22",
                                            NA)),
                              exclude_greater_than = as.Date("0001-12-12"))$exclusions$rel_excluded , 0.9)
  expect_equal(perform_dqtest(x = as.Date(c("2000-12-12",
                                            rep("3000-01-01",7),
                                            "0110-02-22",
                                            NA)),
                              exclude_greater_than = as.Date("0001-12-12"))$exclusions$abs_excluded , 9)
  expect_equal(perform_dqtest(x = as.Date(c("2000-12-12",
                                            rep("3000-01-01",7),
                                            "0110-02-22",
                                            NA)),
                              exclude_greater_than = as.Date("0001-12-12"))$pre_scheduled_return , "Yes, all not missing excluded.")
})


test_that("Exclusions datetime", {
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-12-12 00:12:33",
                                               rep("3000-01-01 11:11:23",7),
                                               "0110-02-22 00:00:01",
                                               NA)))$exclusions , "No exclusions.")
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-12-12 00:12:33",
                                               rep("3000-01-01 11:11:23",7),
                                               "0110-02-22 00:00:01",
                                               NA)),
                              exclude_values = as.POSIXct("1999-01-01 00:00:00"))$exclusions , "No exclusions.")
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-12-12 00:12:33",
                                               rep("3000-01-01 11:11:23",7),
                                               "0110-02-22 00:00:01",
                                               NA)),
                              exclude_smaller_than = as.POSIXct("0022-01-01 00:00:00"))$exclusions , "No exclusions.")
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-12-12 00:12:33",
                                               rep("3000-01-01 11:11:23",7),
                                               "0110-02-22 00:00:01",
                                               NA)),
                              exclude_greater_than = as.POSIXct("9999-01-01 00:00:00"))$exclusions , "No exclusions.")
  expect_error(perform_dqtest(x = as.POSIXct(c("2000-12-12 00:12:33",
                                               rep("3000-01-01 11:11:23",7),
                                               "0110-02-22 00:00:01",
                                               NA)),
                              exclude_greater_than = "4000-01-01 00:00:00")$exclusions)
  expect_error(perform_dqtest(x = as.POSIXct(c("2000-12-12 00:12:33",
                                               rep("3000-01-01 11:11:23",7),
                                               "0110-02-22 00:00:01",
                                               NA)),
                              exclude_smaller_than = NA)$exclusions)
  expect_error(perform_dqtest(x = as.POSIXct(c("2000-12-12 00:12:33",
                                               rep("3000-01-01 11:11:23",7),
                                               "0110-02-22 00:00:01",
                                               NA)),
                              exclude_values = "4000-01-01")$exclusions)
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-12-12 00:12:33",
                                               rep("3000-01-01 11:11:23",7),
                                               "0110-02-22 00:00:01",
                                               NA)),
                              exclude_values = as.POSIXct("2000-12-12 00:12:33"))$exclusions$abs_excluded , 1)
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-12-12 00:12:33",
                                               rep("3000-01-01 11:11:23",7),
                                               "0110-02-22 00:00:01",
                                               NA)),
                              exclude_values = as.POSIXct("3000-01-01 11:11:23"))$exclusions$abs_excluded , 7)
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-12-12 00:12:33",
                                               rep("3000-01-01 11:11:23",7),
                                               "0110-02-22 00:00:01",
                                               NA)),
                              exclude_values = as.POSIXct("2000-12-12 00:12:33"))$exclusions$rel_excluded , 0.1)
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-12-12 00:12:33",
                                               rep("3000-01-01 11:11:23",7),
                                               "0110-02-22 00:00:01",
                                               NA)),
                              exclude_values = as.POSIXct(c("3000-01-01 11:11:23","0110-02-22 00:00:01")))$exclusions$rel_excluded , 0.8)
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-12-12 00:12:33",
                                               rep("3000-01-01 11:11:23",7),
                                               "0110-02-22 00:00:01",
                                               NA)),
                              exclude_greater_than = as.POSIXct("2000-12-12 00:12:33"))$exclusions$rel_excluded , 0.7)
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-12-12 00:12:33",
                                               rep("3000-01-01 11:11:23",7),
                                               "0110-02-22 00:00:01",
                                               NA)),
                              exclude_greater_than = as.POSIXct("0001-12-12"))$exclusions$rel_excluded , 0.9)
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-12-12 00:12:33",
                                               rep("3000-01-01 11:11:23",7),
                                               "0110-02-22 00:00:01",
                                               NA)),
                              exclude_greater_than = as.POSIXct("0001-12-12"))$exclusions$abs_excluded , 9)
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-12-12 00:12:33",
                                               rep("3000-01-01 11:11:23",7),
                                               "0110-02-22 00:00:01",
                                               NA)),
                              exclude_greater_than = as.POSIXct("0001-12-12"))$pre_scheduled_return , "Yes, all not missing excluded.")
})




