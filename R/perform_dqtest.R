


perform_dqtest <- function(x,
                           var_name = "", # vaiable name for nicer plots
                           cat_thres = 30, # int value
                           categories = NULL, # character vector
                           range_min = NULL,
                           range_max = NULL,
                           exclude_values = NULL, #numeric value for continuous data # POISXct for datetime or Date for date #character for cat
                           exclude_smaller_than = NULL, #numeric value for continuous data # POISXct for datetime or Date for date
                           exclude_greater_than = NULL #numeric value for continuous data # POISXct for datetime or Date for date
                           ){
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
  } else if(classification == "datetime"){ #################################################################################
    # convert into POSIXct (from potentially POSIXlt)
    x <- as.POSIXct(x)
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
  } else{
    stop(paste("Class of the input vector not specified!"))
  }
  # return the final list of results
  return(output_list)
}















