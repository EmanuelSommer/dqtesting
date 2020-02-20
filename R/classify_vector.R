#' Determine whether the vector will be classified as: continuous, categorical, datetime or date
#'
#' @param x  a single vector (the allowed classes are: numeric, integer, factor, logical, character, POSIXct/ POSIXlt for datetimes and Date for dates.)
#' @param cat_thres non negative integer value to set the number of unique values a numeric or integer field needs to have to be classified as continuous. (default is set to 30)
#'
#' @return a character string with either "cont", "cat", "datetime or "date"
#' @export
#'
#' @author Emanuel Sommer
classify_vector <- function(x, cat_thres = 30){
  if(!is.numeric(cat_thres)){
    stop("The cat_thres argument must be numeric!")
  } else if(cat_thres < 0){
    stop("The cat_thres argument must be non negative!")
  }
  act_class <- class(x)
  num_unique <- length(unique(x))

  if(("numeric" %in% act_class && num_unique > cat_thres) |
      ("integer" %in% act_class && num_unique > cat_thres)){
    return("cont")
  } else if ("character" %in% act_class | "factor" %in% act_class   |
              ("integer" %in% act_class && num_unique <= cat_thres) |
              ("numeric" %in% act_class && num_unique <= cat_thres) |
              "logical"%in% act_class){
    return("cat")
  } else if ("POSIXct" %in% act_class | "POSIXlt" %in% act_class){
    return("datetime")
  } else if ("Date" %in% act_class){
    return("date")
  } else {
    stop(paste("Class of the input vector not specified:",act_class))
  }
}

