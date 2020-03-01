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

test_that("Class type", {
  expect_equal(perform_dqtest(x = c(1:10,NA))$class_type , "integer")
  expect_equal(perform_dqtest(x = c(1:10,NA,0.1))$class_type , "numeric")
  expect_equal(perform_dqtest(x = c("abc",NA))$class_type , "character")
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-12-12 00:12:33",
                                               "3000-01-01 11:11:23",
                                               "0110-02-22 00:00:01",
                                               NA)))$class_type , c("POSIXct", "POSIXt"))
  expect_equal(perform_dqtest(x = as.Date(c("2000-12-12",
                                               "3000-01-01",
                                               "0110-02-22",
                                               NA)))$class_type , "Date")
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

##############################################################################################################
#
#                        Summary tests
#
##############################################################################################################

test_that("Summary continuous", {
  expect_equal(perform_dqtest(x = c(1:100,NA))$stat_summary$mean , 50.5)
  expect_equal(perform_dqtest(x = c(1:100,NA,0.1))$stat_summary$mean , 50.00099)
  expect_equal(round(perform_dqtest(x = c(1:100,NA,0.1))$stat_summary$sd,5) , 29.29847)
  expect_equal(perform_dqtest(x = c(1:100,NA,0.1))$stat_summary$quantiles11 , c(0.1,10.0,20.0,30.0,40.0,50.0,60.0,70.0,80.0,90.0,100.0))
  expect_equal(perform_dqtest(x = c(1:100,NA,0.1))$stat_summary$skew , 0.0001942934)
})

test_that("Summary categorical", {
  expect_equal(perform_dqtest(x = c(NA,rep("abc",4),rep("def",3),"xyz","test"))$stat_summary$x , c("abc","def","test","xyz",NA))
  expect_equal(perform_dqtest(x = c(NA,rep("abc",4),rep("def",3),"xyz","test"))$stat_summary$absolute , c(4,3,1,1,1))
  expect_equal(perform_dqtest(x = c(NA,rep("abc",4),rep("def",3),"xyz","test"))$stat_summary$relative , c(0.4,0.3,0.1,0.1,0.1))
})

test_that("Summary date", {
  expect_equal(lubridate::day(perform_dqtest(x = as.Date(c("2000-01-01",rep("2001-02-01",10),"2010-11-11")))$stat_summary$mean),
               lubridate::day(as.Date("2001-10-23")))
  expect_equal(lubridate::month(perform_dqtest(x = as.Date(c("2000-01-01",rep("2001-02-01",10),"2010-11-11")))$stat_summary$mean),
               lubridate::month(as.Date("2001-10-23")))
  expect_equal(lubridate::year(perform_dqtest(x = as.Date(c("2000-01-01",rep("2001-02-01",10),"2010-11-11")))$stat_summary$mean),
               lubridate::year(as.Date("2001-10-23")))
  expect_equal(lubridate::day(perform_dqtest(x = as.Date(c("2000-01-01",rep("2001-02-01",10),"2010-11-11")))$stat_summary$quantiles11[6]),
               lubridate::day(as.Date("2001-02-01")))
  expect_equal(lubridate::month(perform_dqtest(x = as.Date(c("2000-01-01",rep("2001-02-01",10),"2010-11-11")))$stat_summary$quantiles11[6]),
               lubridate::month(as.Date("2001-02-01")))
  expect_equal(lubridate::year(perform_dqtest(x = as.Date(c("2000-01-01",rep("2001-02-01",10),"2010-11-11")))$stat_summary$quantiles11[6]),
               lubridate::year(as.Date("2001-02-01")))
})

test_that("Summary datetime", {
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-01-01 00:11:32",rep("2001-02-01 11:02:00",10),"2010-11-11")))$stat_summary$mean , as.POSIXct("2001-10-23 11:00:00"))
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-01-01 00:11:32",rep("2001-02-01 11:02:00",10),"2010-11-11")))$stat_summary$quantiles11 , as.POSIXct(c("2000-01-01 CET", "2001-02-01 CET",
                                                                                                                                                         "2001-02-01 CET", "2001-02-01 CET",
                                                                                                                                                         "2001-02-01 CET", "2001-02-01 CET",
                                                                                                                                                         "2001-02-01 CET", "2001-02-01 CET",
                                                                                                                                                         "2001-02-01 CET", "2001-02-01 CET",
                                                                                                                                                         "2010-11-11 CET")))
})

##############################################################################################################
#
#                        Checks tests
#
##############################################################################################################

test_that("Range check continuous", {
  expect_equal(perform_dqtest(x = c(1:100,NA))$range_check , "No ranges were supplied.")
  expect_equal(perform_dqtest(x = c(1:100,NA), range_min = 1, range_max = 100)$range_check,
               "In range.")
  expect_equal(perform_dqtest(x = c(1:100,NA), range_min = 3, range_max = 100)$range_check,
               "Minimum out of range.")
  expect_equal(perform_dqtest(x = c(1:100,NA), range_min = -111, range_max = 99)$range_check,
               "Maximum out of range.")
  expect_equal(perform_dqtest(x = c(1:100,NA), range_min = 11, range_max = 99)$range_check,
               "Maximum and minimum out of range.")
  expect_error(perform_dqtest(x = c(1:100,NA), range_min = "11", range_max = 99)$range_check)
})

test_that("Categories check", {
  expect_equal(perform_dqtest(x = c(NA,rep("abc",4),rep("def",3),"xyz","test"))$cat_check ,
               "No categories were specified.")
  expect_equal(perform_dqtest(x = c(NA,rep("abc",4),rep("def",3),"xyz","test"),
                              categories = c("gg"))$cat_check ,
               "There are unspecified categories!")
  expect_equal(perform_dqtest(x = c(NA,rep("abc",4),rep("def",3),"xyz","test"),
                              categories = c("abc","defg"))$cat_check ,
               "There are unspecified categories!")
  expect_equal(perform_dqtest(x = c(NA,rep("abc",4),rep("def",3),"xyz","test"),
                              categories = c("abc","def","test","xyz"))$cat_check ,
               "Only the required categories are present.")
  expect_equal(perform_dqtest(x = c(NA,rep("abc",4),rep("def",3),"xyz","test"),
                              categories = c("abc","def","test","xyz","ghi"))$cat_check ,
               "Only the required categories are present.")
  expect_error(perform_dqtest(x = c(NA,rep("abc",4),rep("def",3),"xyz","test"),
                              categories = 22)$cat_check)
})

test_that("Range check date", {
  expect_equal(perform_dqtest(x = as.Date(c("2000-01-01",rep("2001-02-01",10),"2010-11-11")))$range_check , "No ranges were supplied.")
  expect_equal(perform_dqtest(x = as.Date(c("2000-01-01",rep("2001-02-01",10),"2010-11-11")),
                              range_min = as.Date("2000-01-01"), range_max = as.Date("2010-11-11"))$range_check,
               "In range.")
  expect_equal(perform_dqtest(x = as.Date(c("2000-01-01",rep("2001-02-01",10),"2010-11-11")),
                              range_min = as.Date("2000-01-02"), range_max = as.Date("2010-11-11"))$range_check,
               "Minimum out of range.")
  expect_equal(perform_dqtest(x = as.Date(c("2000-01-01",rep("2001-02-01",10),"2010-11-11")),
                              range_min = as.Date("2000-01-01"), range_max = as.Date("2010-11-10"))$range_check,
               "Maximum out of range.")
  expect_equal(perform_dqtest(x = as.Date(c("2000-01-01",rep("2001-02-01",10),"2010-11-11")),
                              range_min = as.Date("2000-02-01"), range_max = as.Date("2010-10-10"))$range_check,
               "Maximum and minimum out of range.")
  expect_error(perform_dqtest(x = as.Date(c("2000-01-01",rep("2001-02-01",10),"2010-11-11")),
                              range_min = "2000-01-01", range_max = as.Date("2010-11-10"))$range_check)
})

test_that("Range check datetime", {
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-01-01 00:11:32",rep("2001-02-01 11:02:00",10),"2010-11-11"), tz = "UTC"))$range_check ,
               "No ranges were supplied.")
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-01-01 00:11:32",rep("2001-02-01 11:02:00",10),"2010-11-11"), tz = "UTC"),
                              range_min = as.POSIXct("1999-12-31 00:11:01", tz = "UTC"), range_max = as.POSIXct("2010-11-11 11:02:00", tz = "UTC"))$range_check,
               "In range.")
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-01-01 23:11:32",rep("2001-02-01 11:02:00",10),"2010-11-11 00:00:00"), tz = "UTC"),
                              range_min = as.POSIXct("2000-01-01 00:00:01", tz = "UTC"), range_max = as.POSIXct("2010-11-11 11:02:00", tz = "UTC"))$range_check,
               "In range.")
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-01-01 00:11:32",rep("2001-02-01 11:02:00",10),"2010-11-11"), tz = "UTC"),
                              range_min = as.POSIXct("2000-01-02", tz = "UTC"), range_max = as.POSIXct("2010-11-12", tz = "UTC"))$range_check,
               "Minimum out of range.")
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-01-01 00:11:32",rep("2001-02-01 11:02:00",10),"2010-11-11"), tz = "UTC"),
                              range_min = as.POSIXct("2000-01-01", tz = "UTC"), range_max = as.POSIXct("2010-11-10", tz = "UTC"))$range_check,
               "Maximum out of range.")
  expect_equal(perform_dqtest(x = as.POSIXct(c("2000-01-01 00:11:32",rep("2001-02-01 11:02:00",10),"2010-11-11"), tz = "UTC"),
                              range_min = as.POSIXct("2000-02-02", tz = "UTC"), range_max = as.POSIXct("2010-10-10", tz = "UTC"))$range_check,
               "Maximum and minimum out of range.")
  expect_error(perform_dqtest(x = as.POSIXct(c("2000-01-01 00:11:32",rep("2001-02-01 11:02:00",10),"2010-11-11"), tz = "UTC"),
                              range_min = "2000-01-01", range_max = as.Date("2010-11-10"))$range_check)
})



##############################################################################################################
#
#                        Checks plots
#
##############################################################################################################

test_that("continuous plots are ggplots", {
  expect_true(ggplot2::is.ggplot(perform_dqtest(x = c(1:100,NA), range_min = 11, range_max = 99)$boxplot))
  expect_true(ggplot2::is.ggplot(perform_dqtest(x = c(1:100,NA), range_min = 11, range_max = 99)$hist))
})

test_that("categorical plot is ggplot", {
  expect_true(ggplot2::is.ggplot(perform_dqtest(x = c(NA,rep("abc",4),rep("def",3),"xyz","test"),
                                                categories = c("abc","def","test","xyz","ghi"), rel = FALSE,flip_axis = FALSE)$barplot))
  expect_true(ggplot2::is.ggplot(perform_dqtest(x = c(NA,rep("abc",4),rep("def",3),"xyz","test"),
                                                categories = c("abc","def","test","xyz","ghi"), rel = TRUE,flip_axis = TRUE)$barplot))
})

test_that("date plots are ggplots", {
  expect_true(ggplot2::is.ggplot(perform_dqtest(x = as.Date(c("2000-01-01",rep("2001-02-01",10),"2010-11-11")),
                                                range_min = as.Date("2000-01-01"), range_max = as.Date("2010-11-10"))$hist_date))
  expect_true(ggplot2::is.ggplot(perform_dqtest(x = as.Date(c("2000-01-01",rep("2001-02-01",10),"2010-11-11")),
                                                range_min = as.Date("2000-01-01"), range_max = as.Date("2010-11-10"))$hist_year))
  expect_true(ggplot2::is.ggplot(perform_dqtest(x = as.Date(c("2000-01-01",rep("2001-02-01",10),"2010-11-11")),
                                                range_min = as.Date("2000-01-01"), range_max = as.Date("2010-11-10"))$hist_month))
  expect_true(ggplot2::is.ggplot(perform_dqtest(x = as.Date(c("2000-01-01",rep("2001-02-01",10),"2010-11-11")),
                                                range_min = as.Date("2000-01-01"), range_max = as.Date("2010-11-10"))$hist_week))
  expect_true(ggplot2::is.ggplot(perform_dqtest(x = as.Date(c("2000-01-01",rep("2001-02-01",10),"2010-11-11")),
                                                range_min = as.Date("2000-01-01"), range_max = as.Date("2010-11-10"))$hist_wday))
  expect_true(ggplot2::is.ggplot(perform_dqtest(x = as.Date(c("2000-01-01",rep("2001-02-01",10),"2010-11-11")),
                                                range_min = as.Date("2000-01-01"), range_max = as.Date("2010-11-10"))$hist_day))
})

test_that("datetime plots are ggplots", {
  expect_true(ggplot2::is.ggplot(perform_dqtest(x = as.POSIXct(c("2000-01-01 00:11:32",rep("2001-02-01 11:02:00",10),"2010-11-11"), tz = "UTC"),
                                                range_min = as.POSIXct("2000-01-02", tz = "UTC"), range_max = as.POSIXct("2010-11-12", tz = "UTC"))$hist_datetime))
  expect_true(ggplot2::is.ggplot(perform_dqtest(x = as.POSIXct(c("2000-01-01 00:11:32",rep("2001-02-01 11:02:00",10),"2010-11-11"), tz = "UTC"),
                                                range_min = as.POSIXct("2000-01-02", tz = "UTC"), range_max = as.POSIXct("2010-11-12", tz = "UTC"))$hist_year))
  expect_true(ggplot2::is.ggplot(perform_dqtest(x = as.POSIXct(c("2000-01-01 00:11:32",rep("2001-02-01 11:02:00",10),"2010-11-11"), tz = "UTC"),
                                                range_min = as.POSIXct("2000-01-02", tz = "UTC"), range_max = as.POSIXct("2010-11-12", tz = "UTC"))$hist_month))
  expect_true(ggplot2::is.ggplot(perform_dqtest(x = as.POSIXct(c("2000-01-01 00:11:32",rep("2001-02-01 11:02:00",10),"2010-11-11"), tz = "UTC"),
                                                range_min = as.POSIXct("2000-01-02", tz = "UTC"), range_max = as.POSIXct("2010-11-12", tz = "UTC"))$hist_week))
  expect_true(ggplot2::is.ggplot(perform_dqtest(x = as.POSIXct(c("2000-01-01 00:11:32",rep("2001-02-01 11:02:00",10),"2010-11-11"), tz = "UTC"),
                                                range_min = as.POSIXct("2000-01-02", tz = "UTC"), range_max = as.POSIXct("2010-11-12", tz = "UTC"))$hist_wday))
  expect_true(ggplot2::is.ggplot(perform_dqtest(x = as.POSIXct(c("2000-01-01 00:11:32",rep("2001-02-01 11:02:00",10),"2010-11-11"), tz = "UTC"),
                                                range_min = as.POSIXct("2000-01-02", tz = "UTC"), range_max = as.POSIXct("2010-11-12", tz = "UTC"))$hist_day))
  expect_true(ggplot2::is.ggplot(perform_dqtest(x = as.POSIXct(c("2000-01-01 00:11:32",rep("2001-02-01 11:02:00",10),"2010-11-11"), tz = "UTC"),
                                                range_min = as.POSIXct("2000-01-02", tz = "UTC"), range_max = as.POSIXct("2010-11-12", tz = "UTC"))$hist_hour))
  expect_true(ggplot2::is.ggplot(perform_dqtest(x = as.POSIXct(c("2000-01-01 00:11:32",rep("2001-02-01 11:02:00",10),"2010-11-11"), tz = "UTC"),
                                                range_min = as.POSIXct("2000-01-02", tz = "UTC"), range_max = as.POSIXct("2010-11-12", tz = "UTC"))$hist_minute))
  expect_true(ggplot2::is.ggplot(perform_dqtest(x = as.POSIXct(c("2000-01-01 00:11:32",rep("2001-02-01 11:02:00",10),"2010-11-11"), tz = "UTC"),
                                                range_min = as.POSIXct("2000-01-02", tz = "UTC"), range_max = as.POSIXct("2010-11-12", tz = "UTC"))$hist_second))
})




