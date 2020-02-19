
# imports    tibble dyplr

perform_dqtest <- function(x,
                           var_name = "", # vaiable name for nicer plots
                           cat_thres = 30, # int value
                           categories = NULL, # character vector
                           range_min = NULL,# else Date minimum 0000-01-01, max 9999-12-31
                           range_max = NULL,
                           tz = NULL, # can exchange the timezone else timezone of the input
                           exclude_values = NULL, #numeric value for continuous data # POISXct for datetime or Date for date #character for cat
                           exclude_smaller_than = NULL, #numeric value for continuous data # POISXct for datetime or Date for date
                           exclude_greater_than = NULL, #numeric value for continuous data # POISXct for datetime or Date for date
                           plot_col1 = "darkviolet", #primary plot color
                           plot_col2 = NULL, # secondary plot color (for example for range bounds, or unallowed categories)
                           bin_num = 100, #The number of bins of the histogram (for continuous, date and datetime input). Must be given as a positive integer value.
                           alpha = 0.8, #A numeric value between 0 and 1 to adjust the transparancy of the plots.
                           rel = FALSE, #Logical value indicating whether the count or the density should be shown on the y axis of the diagnostic plots)
                           orig_proportions = TRUE #logical value indicating whether for categorical input in the relative barplot the length before exclutions should be used for the calculation of the proportions. If TRUE the original length will be used.
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
      sd = sd(x, na.rm = TRUE),
      quantiles11 = quantile(x, na.rm = TRUE, probs = seq(0,1,0.1), names = FALSE),
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
                                     rel = rel)
    } else {
      output_list$barplot <- bar_cat(x_factor = x_factor,
                                     var_name = var_name,
                                     categories = categories,
                                     color1 = plot_col1,
                                     color2 = ifelse(is.null(plot_col2),"yellow2",plot_col2),
                                     alpha = alpha,
                                     rel = rel)
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
      quantiles11 = lubridate::date(quantile(as.POSIXct(x), na.rm = TRUE, probs = seq(0,1,0.1), names = FALSE))
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
      quantiles11 = quantile(x, na.rm = TRUE, probs = seq(0,1,0.1), names = FALSE)
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















