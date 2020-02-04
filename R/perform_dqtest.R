


perform_dqtest <- function(x,
                           var_name = "",
                           cat_thres = 30,
                           categories = NULL,
                           range_min = NULL,
                           range_max = NULL,
                           exclude_values = NULL,
                           exclude_smaller_than = NULL,
                           exclude_greater_than = NULL
                           ){
  # check whether the vector x contains only missing values. In this case a further analysis is worthless.
  if(all(is.na(x))){
    return(list(all_na = TRUE))
  } else {
    output_list <- list(all_na = FALSE) # initiate output list with the first bit of information
  }
  # classify the vector + general statistics for later on (before any exclutions are made)
  classification <- classify_vector(x = x, cat_thres = cat_thres)
  length_original <- length(x)
  abs_na <- sum(is.na(x))
  rel_na <- abs_na/length_original
  # add information to output list
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


  } else if(classification == "cat"){ ######################################################################################
    #convert to a factor
    x_factor <- as.factor(x)
  } else if(classification == "date"){ #####################################################################################

  } else if(classification == "datetime"){ #################################################################################

  } else{
    stop(paste("Class of the input vector not specified!"))
  }
  # return the final list of results
  return(output_list)
}















