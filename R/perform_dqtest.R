#' Univariate data quality testing.
#'
#' The function perform_dqtest supplies the user with a list that contains the most important information to  perform  a  univariate data quality test.
#'
#' \itemize{
#'    \item{The elements of the list vary depending on the input except for the element \code{pre_scheduled_return} which indicates whether all the values of x are missing values (-> "Yes, all NA."), or whether all values of x were excluded prior to the analysis (-> "Yes, all not missing excluded.") else the element contains "No.".}
#'    \item{The function first of all classifies the input vector with the help of the \code{\link{classify_vector}} function and further analysis is performed depending on this classification. One can access the classification via the \code{classification} element of the list.}
#'    \item{The class type of the initial vector (like numeric or POSIXct) can be accessed via the \code{class_type} element.}
#'    \item{The original length of the input vector can be accessed via the \code{initial_length} element.}
#'    \item{The absolute and relative amount of NAs (missing values) can be accessed via the \code{abs_na} and \code{rel_na} elements.}
#' }
#'
#' The other elements of the list are then dependend on the initial classification:
#' \describe{
#'    \item{\strong{continuous}}{\itemize{
#'    \item{The \code{exclusions} element either contains the character value "No exclusions." or a list with the relative and absolute amount of exclusions.}
#'    \item{The \code{stat_summary} element contains a list with the mean, standard deviation, skewness and the seq(0,1,0.1) quantiles (11).}
#'    \item{The \code{range_check} element contains a character value the tells the user about the result of the range check.}
#'    \item{The \code{boxplot} and \code{hist} return two ggplot objects for diagnostic purposes.}}}
#'    \item{\strong{categorical}}{\itemize{
#'    \item{The \code{exclusions} element either contains the character value "No exclusions." or a list with the relative and absolute amount of exclusions.}
#'    \item{The \code{stat_summary} element contains a tibble with the rows representing all unique categories and with the columns showing the absolute and relative appearances ordered descending by absolute appearances.}
#'    \item{The \code{cat_check} element contains a character value the tells the user about the result of the categories check (whether all actual categories are included in the supplied ones).}
#'    \item{The \code{barplot} returns a ggplot object for diagnostic purposes.}}}
#'    \item{\strong{date}}{\itemize{
#'    \item{The \code{exclusions} element either contains the character value "No exclusions." or a list with the relative and absolute amount of exclusions.}
#'    \item{The \code{stat_summary} element contains a list with the mean and the seq(0,1,0.1) quantiles (11).}
#'    \item{The \code{range_check} element contains a character value the tells the user about the result of the range check.}
#'    \item{The \code{hist_date},\code{hist_year}, \code{hist_month}, \code{hist_week},\code{hist_wday} and \code{hist_day} return ggplot objects for diagnostic purposes.}}}
#'    \item{\strong{datetime}}{\itemize{
#'    \item{The \code{exclusions} element either contains the character value "No exclusions." or a list with the relative and absolute amount of exclusions.}
#'    \item{The \code{stat_summary} element contains a list with the mean and the seq(0,1,0.1) quantiles (11).}
#'    \item{The \code{range_check} element contains a character value the tells the user about the result of the range check.}
#'    \item{The \code{hist_date},\code{hist_year}, \code{hist_month}, \code{hist_week},\code{hist_wday}, \code{hist_day}, \code{hist_hour}, \code{hist_minute} and \code{hist_second} return ggplot objects for diagnostic purposes.}}}
#' }
#'
#' @param x A single vector (the allowed classes are: numeric, integer, factor, logical, character, POSIXct/ POSIXlt for datetimes and Date for dates.)
#' @param var_name The variable name to be used in diagnostic plots. Must be given as character.
#' @param cat_thres A non negative integer value to set the number of unique values a numeric or integer vector needs to have to be classified as continuous. (default is set to 30)
#' @param categories A character vector of categories. For categorical input the categories check will be based on this vector.
#' @param range_min For continuous input data a numeric, for Date input a Date and for datetime input a POSIXct value. The value is themn used in the range check as the lower range bound.
#' @param range_max For continuous input data a numeric, for Date input a Date and for datetime input a POSIXct value. The value is themn used in the range check as the upper range bound.
#' @param tz If the output should be in another timezone than the input data, one can set the timezone here by supplying a character vector with a valid timezone shortcut.
#' @param exclude_values For continuous input data a numeric, for categorical input a character, for Date input a Date and for datetime input a POSIXct vector. The values of this vector will be excluded prior to the analysis.
#' @param exclude_smaller_than For continuous input data a numeric, for Date input a Date and for datetime input a POSIXct value. values strictly smaller than this value will be excluded prior to the analysis.
#' @param exclude_greater_than For continuous input data a numeric, for Date input a Date and for datetime input a POSIXct value. values strictly greater than this value will be excluded prior to the analysis.
#' @param plot_col1 The primary plot color. Must be given as character.
#' @param plot_col2 The secondary plot color. For example for range bounds or unallowed categories. Must be given as character.
#' @param bin_num The number of bins of the histogram (for continuous, date and datetime input). Must be given as a positive integer value.
#' @param alpha A numeric value between 0 and 1 to adjust the transparancy of the plots.
#' @param rel A logical value indicating whether the count or the density should be shown on the y axis of the diagnostic plots. (default: count)
#' @param orig_proportions A logical value indicating whether for categorical input in the relative barplot the length before exclutions should be used for the calculation of the proportions. If TRUE the original length will be used.
#' @param flip_axis Logical value indicating whether in the barplot for categorical variables the categories should be shown on the x or y axis. (default: x axis) Especially for long category names one should consider the axis flip.
#'
#' @return A list. More details above.
#' @export
#'
#' @import dplyr
#' @import tibble
#'
#' @author Emanuel Sommer
#' @seealso \code{\link{classify_vector}}, \code{\link{bar_cat}}, \code{\link{cont_boxplot}}, \code{\link{hist_cont}}, \code{\link{hist_date}}, \code{\link{hist_datetime}}
perform_dqtest <- function(x,
                           var_name = "",
                           cat_thres = 30,
                           categories = NULL,
                           range_min = NULL,
                           range_max = NULL,
                           tz = NULL,
                           exclude_values = NULL,
                           exclude_smaller_than = NULL,
                           exclude_greater_than = NULL,
                           plot_col1 = "darkviolet",
                           plot_col2 = NULL,
                           bin_num = 100,
                           alpha = 0.8,
                           rel = FALSE,
                           orig_proportions = TRUE,
                           flip_axis = FALSE
                           ){
  #check for valid input
  if(!is.null(dim(x)) && !is.list(x)){stop("x must be a single vector")}
  if(!is.character(var_name)){stop("If supplied the var_name argument must be character.")}
  if(!(is.character(tz) | is.null(tz))){stop("If supplied the tz argument must be character.")}
  # check whether the vector x contains only missing values. In this case a further analysis is worthless.
  if(all(is.na(x))){
    return(list(pre_scheduled_return = "Yes, all NA."))
  } else {
    output_list <- list(pre_scheduled_return = "No.") # initiate output list with the first bit of information
  }
  # classify the vector + general statistics for later on (before any exclutions are made)
  classification <- classify_vector(x = x, cat_thres = cat_thres)
  length_original <- length(x)
  abs_na <- sum(is.na(x))
  rel_na <- abs_na/length_original
  # add information to output list
  output_list$classification <- classification
  output_list$class_type <- class(x)
  output_list$initial_length <- length_original
  output_list$abs_na <- abs_na
  output_list$rel_na <- rel_na
  # start the individual analysis for each of the categories of classification
  if(classification == "cont"){ ############################################################################################
    # exclude values if requested
    if(!is.null(exclude_values)){
      if(!is.numeric(exclude_values)){stop("For continuous input the values to be excluded must be numeric.")}
      x <- x[!(x %in% exclude_values)]
    }
    if(!is.null(exclude_smaller_than)){
      if(!is.numeric(exclude_smaller_than)){stop("For continuous input the bounds for values to be excluded must be numeric.")}
      x <- x[!(x < exclude_smaller_than)]
    }
    if(!is.null(exclude_greater_than)){
      if(!is.numeric(exclude_greater_than)){stop("For continuous input the bounds for values to be excluded must be numeric.")}
      x <- x[!(x > exclude_greater_than)]
    }
    # add the information of the exclusions process to the output list
    if(length(x) == length_original){
      output_list$exclusions <- "No exclusions."
    } else {
      output_list$exclusions <- list(rel_excluded = 1 - (length(x)/length_original),
                                     abs_excluded = length_original -length(x))
    }
    if(length(x[!is.na(x)]) == 0){
      output_list$pre_scheduled_return <- "Yes, all not missing excluded."
      return(output_list)
    }
    # summary statistics
    summary_cont_list <- list(
      mean = mean(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      quantiles11 = stats::quantile(x, na.rm = TRUE, probs = seq(0,1,0.1), names = FALSE),
      skew = e1071::skewness(x, na.rm = TRUE)
    )
    output_list$stat_summary <- summary_cont_list
    # range check: (if ranges were supplied)
    if(is.null(range_min) && is.null(range_max)){
      output_list$range_check <- "No ranges were supplied."
    } else {
      if(!(is.null(range_min) | is.numeric(range_min))){stop("The ranges for continuous input must be numeric if given.")}
      if(!(is.null(range_max) | is.numeric(range_max))){stop("The ranges for continuous input must be numeric if given.")}
      # treat cases where only one range was supplied:
      range_min <- dplyr::if_else(is.null(range_min), -Inf, range_min)
      range_max <- dplyr::if_else(is.null(range_max), Inf, range_max)
      act_min <- min(x,na.rm = T)
      act_max <- max(x,na.rm = T)
      if(act_min < range_min && act_max <= range_max){
        range_check_num <- "Minimum out of range."
      } else if (act_min >= range_min && act_max > range_max){
        range_check_num <- "Maximum out of range."
      } else if (act_min < range_min && act_max > range_max){
        range_check_num <- "Maximum and minimum out of range."
      } else {
        range_check_num <- "In range."
      }
      output_list$range_check <- range_check_num
    }
    # diagnostic plots:
    output_list$boxplot <- cont_boxplot(x = x,
                                        var_name = var_name,
                                        color = plot_col1)
    output_list$hist <- hist_cont(x = x,
                                  var_name = var_name,
                                  color = plot_col1,
                                  range_col = ifelse(is.null(plot_col2),"black",plot_col2),
                                  bin_num = bin_num,
                                  alpha = alpha,
                                  range_min = range_min,
                                  range_max = range_max,
                                  rel = rel)
  } else if(classification == "cat"){ ######################################################################################
    #convert to a factor
    x_factor <- as.factor(x)
    # exclude values if requested
    if(!is.null(exclude_values)){
      if(!is.character(exclude_values)){stop("exclude_values must be a character vector.")}
      x_factor <- x_factor[!(x_factor %in% exclude_values)]
    }
    # add the information of the exclusions process to the output list
    if(length(x_factor) == length_original){
      output_list$exclusions <- "No exclusions."
    } else {
      output_list$exclusions <- list(rel_excluded = 1 - (length(x_factor)/length_original),
                                     abs_excluded = length_original -length(x_factor))
    }
    if(length(x_factor[!is.na(x_factor)]) == 0){
      output_list$pre_scheduled_return <- "Yes, all not missing excluded."
      return(output_list)
    }
    # summary statistics
    summary_tbl <- tibble(x = x_factor) %>%
      mutate(x = as.character(x)) %>%
      count(x) %>%
      rename(absolute = n)%>%
      mutate(relative = absolute/length_original)%>%
      arrange(desc(absolute),x)
    output_list$stat_summary <- summary_tbl
    # categories check
    if(is.null(categories)){
      output_list$cat_check <- "No categories were specified."
    } else {
      if(!is.character(categories)){stop("categories must be supplied as a character vector.")}
      output_list$cat_check <- dplyr::if_else(all(levels(x_factor) %in% categories),
                                      "Only the required categories are present.",
                                      "There are unspecified categories!")
    }
    # diagnostic barplot
    if(orig_proportions){
      output_list$barplot <- bar_cat(x_factor = x_factor,
                                     var_name = var_name,
                                     categories = categories,
                                     length_original = length_original,
                                     color1 = plot_col1,
                                     color2 = ifelse(is.null(plot_col2),"yellow2",plot_col2),
                                     alpha = alpha,
                                     rel = rel,
                                     flip_axis = flip_axis)
    } else {
      output_list$barplot <- bar_cat(x_factor = x_factor,
                                     var_name = var_name,
                                     categories = categories,
                                     color1 = plot_col1,
                                     color2 = ifelse(is.null(plot_col2),"yellow2",plot_col2),
                                     alpha = alpha,
                                     rel = rel,
                                     flip_axis = flip_axis)
    }
  } else if(classification == "date"){ #####################################################################################
    # exclude values if requested
    if(!is.null(exclude_values)){
      if(!lubridate::is.Date(exclude_values)){stop("For Date input the values to be excluded must be given as Date.")}
      x <- x[!(x %in% exclude_values)]
    }
    if(!is.null(exclude_smaller_than)){
      if(!lubridate::is.Date(exclude_smaller_than)){stop("For Date input the bounds for values to be excluded must be Dates")}
      x <- x[!(x < exclude_smaller_than)]
    }
    if(!is.null(exclude_greater_than)){
      if(!lubridate::is.Date(exclude_greater_than)){stop("For Date input the bounds for values to be excluded must be Dates")}
      x <- x[!(x > exclude_greater_than)]
    }
    # add the information of the exclusions process to the output list
    if(length(x) == length_original){
      output_list$exclusions <- "No exclusions."
    } else {
      output_list$exclusions <- list(rel_excluded = 1 - (length(x)/length_original),
                                     abs_excluded = length_original -length(x))
    }
    if(length(x[!is.na(x)]) == 0){
      output_list$pre_scheduled_return <- "Yes, all not missing excluded."
      return(output_list)
    }
    # summary statistics
    summary_date_list <- list(
      mean = mean(x, na.rm = TRUE),
      quantiles11 = lubridate::date(stats::quantile(as.POSIXct(x), na.rm = TRUE, probs = seq(0,1,0.1), names = FALSE))
    )
    output_list$stat_summary <- summary_date_list
    # range check: (if ranges were supplied)
    if(is.null(range_min) && is.null(range_max)){
      output_list$range_check <- "No ranges were supplied."
    } else {
      if(!(is.null(range_min) | lubridate::is.Date(range_min))){stop("The ranges for Date input must be Dates if given.")}
      if(!(is.null(range_max) | lubridate::is.Date(range_max))){stop("The ranges for Date input must be Dates if given.")}
      # treat cases where only one range was supplied:
      range_min <- dplyr::if_else(is.null(range_min), as.Date("0000-01-01"), range_min)
      range_max <- dplyr::if_else(is.null(range_max), as.Date("9999-12-31"), range_max)
      act_min <- min(x,na.rm = T)
      act_max <- max(x,na.rm = T)
      if(act_min < range_min && act_max <= range_max){
        range_check_date <- "Minimum out of range."
      } else if (act_min >= range_min && act_max > range_max){
        range_check_date <- "Maximum out of range."
      } else if (act_min < range_min && act_max > range_max){
        range_check_date <- "Maximum and minimum out of range."
      } else {
        range_check_date <- "In range."
      }
      output_list$range_check <- range_check_date
    }
    # diagnostic plots
    output_list$hist_date <- hist_date(x = x,
                                       var_name = var_name,
                                       color = plot_col1,
                                       range_col = ifelse(is.null(plot_col2),"black",plot_col2),
                                       bin_num = bin_num,
                                       alpha = alpha,
                                       range_min = range_min,
                                       range_max = range_max,
                                       rel = rel)
    output_list$hist_year <- hist_cont(x = lubridate::year(x),
                                       var_name = var_name,
                                       color = plot_col1,
                                       range_col = ifelse(is.null(plot_col2),"black",plot_col2),
                                       bin_num = bin_num,
                                       alpha = alpha,
                                       range_min = ifelse(is.null(range_min),-Inf,lubridate::year(range_min)),
                                       range_max = ifelse(is.null(range_max),Inf,lubridate::year(range_max)),
                                       rel = rel)
    output_list$hist_month <- hist_cont(x = lubridate::month(x),
                                       var_name = var_name,
                                       color = plot_col1,
                                       range_col = ifelse(is.null(plot_col2),"black",plot_col2),
                                       bin_num = bin_num,
                                       alpha = alpha,
                                       range_min = NULL,
                                       range_max = NULL,
                                       rel = rel) + coord_cartesian(xlim = c(1,12)) + scale_x_continuous(breaks = 1:12)
    output_list$hist_week <- hist_cont(x = lubridate::week(x),
                                       var_name = var_name,
                                       color = plot_col1,
                                       range_col = ifelse(is.null(plot_col2),"black",plot_col2),
                                       bin_num = bin_num,
                                       alpha = alpha,
                                       range_min = NULL,
                                       range_max = NULL,
                                       rel = rel) + coord_cartesian(xlim = c(0,53)) + scale_x_continuous(breaks = (1:30)*5)
    output_list$hist_wday <- bar_cat(x_factor = lubridate::wday(x = x, label = TRUE, abbr = FALSE, locale = "us"),
                                     var_name = var_name,
                                     categories = NULL,
                                     length_original = NULL,
                                     color1 = plot_col1,
                                     color2 = "yellow2",
                                     alpha = alpha,
                                     rel = rel)
    output_list$hist_day <- hist_cont(x = lubridate::day(x),
                                      var_name = var_name,
                                      color = plot_col1,
                                      range_col = ifelse(is.null(plot_col2),"black",plot_col2),
                                      bin_num = bin_num,
                                      alpha = alpha,
                                      range_min = NULL,
                                      range_max = NULL,
                                      rel = rel) + coord_cartesian(xlim = c(1,31)) + scale_x_continuous(breaks = (1:20)*2)
  } else if(classification == "datetime"){ #################################################################################
    # convert into POSIXct (from potentially POSIXlt) and adjust timezone
    if(is.null(tz)){
      tz <- lubridate::tz(x)
    }
    x <- as.POSIXct(x, tz = tz)
    # exclude values if requested
    if(!is.null(exclude_values)){
      if(!lubridate::is.POSIXct(exclude_values)){stop("For datetime input the values to be excluded must be given as POSIXct.")}
      x <- x[!(x %in% exclude_values)]
    }
    if(!is.null(exclude_smaller_than)){
      if(!lubridate::is.POSIXct(exclude_smaller_than)){stop("For datetime input the bounds for values to be excluded must be POSIXct.")}
      x <- x[!(x < exclude_smaller_than)]
    }
    if(!is.null(exclude_greater_than)){
      if(!lubridate::is.POSIXct(exclude_greater_than)){stop("For datetime input the bounds for values to be excluded must be POSIXct.")}
      x <- x[!(x > exclude_greater_than)]
    }
    # add the information of the exclusions process to the output list
    if(length(x) == length_original){
      output_list$exclusions <- "No exclusions."
    } else {
      output_list$exclusions <- list(rel_excluded = 1 - (length(x)/length_original),
                                     abs_excluded = length_original -length(x))
    }
    if(length(x[!is.na(x)]) == 0){
      output_list$pre_scheduled_return <- "Yes, all not missing excluded."
      return(output_list)
    }
    # summary statistics
    summary_datetime_list <- list(
      mean = mean(x, na.rm = TRUE),
      quantiles11 = stats::quantile(x, na.rm = TRUE, probs = seq(0,1,0.1), names = FALSE)
    )
    output_list$stat_summary <- summary_datetime_list
    # range check: (if ranges were supplied)
    if(is.null(range_min) && is.null(range_max)){
      output_list$range_check <- "No ranges were supplied."
    } else {
      if(!(is.null(range_min) | lubridate::is.POSIXct(range_min))){stop("The ranges for datetime input must be POSIXct if given.")}
      if(!(is.null(range_max) | lubridate::is.POSIXct(range_max))){stop("The ranges for datetime input must be POSIXct if given.")}
      # treat cases where only one range was supplied:
      range_min <- dplyr::if_else(is.null(range_min), as.POSIXct("0000-01-01 00:00:00",tz = tz), range_min)
      range_max <- dplyr::if_else(is.null(range_max), as.POSIXct("9999-12-31 23:59:59",tz = tz), range_max)
      act_min <- min(x,na.rm = T)
      act_max <- max(x,na.rm = T)
      if(act_min < range_min && act_max <= range_max){
        range_check_datetime <- "Minimum out of range."
      } else if (act_min >= range_min && act_max > range_max){
        range_check_datetime <- "Maximum out of range."
      } else if (act_min < range_min && act_max > range_max){
        range_check_datetime <- "Maximum and minimum out of range."
      } else {
        range_check_datetime <- "In range."
      }
      output_list$range_check <- range_check_datetime
    }
    # diagnostic plots
    output_list$hist_datetime <- hist_datetime(x = x,
                                               var_name = var_name,
                                               color = plot_col1,
                                               range_col = ifelse(is.null(plot_col2),"black",plot_col2),
                                               bin_num = bin_num,
                                               alpha = alpha,
                                               range_min = range_min,
                                               range_max = range_max,
                                               rel = rel)
    output_list$hist_year <- hist_cont(x = lubridate::year(x),
                                       var_name = var_name,
                                       color = plot_col1,
                                       range_col = ifelse(is.null(plot_col2),"black",plot_col2),
                                       bin_num = bin_num,
                                       alpha = alpha,
                                       range_min = ifelse(is.null(range_min),-Inf,lubridate::year(range_min)),
                                       range_max = ifelse(is.null(range_max),Inf,lubridate::year(range_max)),
                                       rel = rel)
    output_list$hist_month <- hist_cont(x = lubridate::month(x),
                                       var_name = var_name,
                                       color = plot_col1,
                                       range_col = ifelse(is.null(plot_col2),"black",plot_col2),
                                       bin_num = bin_num,
                                       alpha = alpha,
                                       range_min = NULL,
                                       range_max = NULL,
                                       rel = rel) + coord_cartesian(xlim = c(1,12)) + scale_x_continuous(breaks = 1:12)
    output_list$hist_week <- hist_cont(x = lubridate::week(x),
                                       var_name = var_name,
                                       color = plot_col1,
                                       range_col = ifelse(is.null(plot_col2),"black",plot_col2),
                                       bin_num = bin_num,
                                       alpha = alpha,
                                       range_min = NULL,
                                       range_max = NULL,
                                       rel = rel) + coord_cartesian(xlim = c(0,53)) + scale_x_continuous(breaks = (1:30)*5)
    output_list$hist_wday <- bar_cat(x_factor = lubridate::wday(x = x, label = TRUE, abbr = FALSE, locale = "us"),
                                     var_name = var_name,
                                     categories = NULL,
                                     length_original = NULL,
                                     color1 = plot_col1,
                                     color2 = "yellow2",
                                     alpha = alpha,
                                     rel = rel)
    output_list$hist_day <- hist_cont(x = lubridate::day(x),
                                      var_name = var_name,
                                      color = plot_col1,
                                      range_col = ifelse(is.null(plot_col2),"black",plot_col2),
                                      bin_num = bin_num,
                                      alpha = alpha,
                                      range_min = NULL,
                                      range_max = NULL,
                                      rel = rel) + coord_cartesian(xlim = c(1,31)) + scale_x_continuous(breaks = (1:20)*2)
    output_list$hist_hour <- hist_cont(x = lubridate::hour(x),
                                       var_name = var_name,
                                       color = plot_col1,
                                       range_col = ifelse(is.null(plot_col2),"black",plot_col2),
                                       bin_num = bin_num,
                                       alpha = alpha,
                                       range_min = NULL,
                                       range_max = NULL,
                                       rel = rel) + coord_cartesian(xlim = c(0,24)) + scale_x_continuous(breaks = 0:24)
    output_list$hist_minute <- hist_cont(x = lubridate::minute(x),
                                         var_name = var_name,
                                         color = plot_col1,
                                         range_col = ifelse(is.null(plot_col2),"black",plot_col2),
                                         bin_num = bin_num,
                                         alpha = alpha,
                                         range_min = NULL,
                                         range_max = NULL,
                                         rel = rel) + coord_cartesian(xlim = c(0,60)) + scale_x_continuous(breaks = (1:20)*5)
    output_list$hist_second <- hist_cont(x = lubridate::second(x),
                                         var_name = var_name,
                                         color = plot_col1,
                                         range_col = ifelse(is.null(plot_col2),"black",plot_col2),
                                         bin_num = bin_num,
                                         alpha = alpha,
                                         range_min = NULL,
                                         range_max = NULL,
                                         rel = rel) + coord_cartesian(xlim = c(0,60)) + scale_x_continuous(breaks = (1:20)*5)
  } else{
    stop(paste("Class of the input vector not specified!"))
  }
  # return the final list of results
  return(output_list)
}















