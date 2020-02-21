#' Local outlier factor (LOF)
#'
#' A function that calculates the Local Outlier Factor scores for a given set of variables with the same length.
#' Moreover the output list contains a boxplot that visualizes the LOF-scores. The goal is to provide an easy
#' interface for multivariate outlier detection.
#'
#' LOF is a nearest neighbors approach where the local densities are compared. The score is basically a division
#' of the density drawn from the k nearest neighbors divided by the density of the point of interest.
#' Hence values greater than 1 are considered more likely to be an anomaly. In this function the k is not set
#' to a specific value. The function uses a tuning vector of values for k, calculates the scores for any of these
#' k and then averages over all of these scores. This yields the final score that can be found in the output list.
#' The tuning vector is set up around the square root of the variable length (+/- 10) and no k under 10 will be used
#' as the inventor of the LOF claims that this would yield to very unrobust scores. For the LOF-Algorithm the
#' implementation of the dbscan package is used. Moreover the gower distance is used as the metric for the knn algorithm as it can also handle c
#' categorical input. In this quite easy and naive implementation rows with NAs will be omitted. This can result in major problems
#' if the percentage of missing values is relevant or if the missing values are missing due to a certain pattern.
#'
#' @param data A tibble or dataframe with the variables to be checked for outliers as the columns (columns must be named)
#' @param col1 The color of the boxplot. Must be given as character.
#'
#'
#' @return A list with the elements scores and boxplot.
#' @export
#'
#' @author Emanuel Sommer
lof_fun <- function(data, col1 = "darkviolet"){
  if(!is.data.frame(data)){stop("The input is not a data frame!")}
  # omit rows with missing values:
  data <- na.omit(data)
  # dimensions of the input:
  dim_data <- dim(data)
  # get the classes of the columns:
  classes <- sapply(1:dim_data[2],function(i){class(data[[names(data)[i]]])})
  # The following is true when the input has to be converted to a factor:
  classes_trans <- dplyr::if_else(classes %in% c("numeric","integer","factor"),FALSE,TRUE)
  new_data <- dplyr::mutate_if(data,classes_trans,as.factor)
  # tuning vector for the k's in the local outlier factor algorithm
  tune_k <- seq(from = max(10,round(sqrt(dim_data[1])) - 10),
                to = round(sqrt(dim_data[1])) + 10,
                by = 1)
  # differenciate between only numeric inputs and mixed ones: (gower distance metric)
  if(all(classes %in% c("numeric","integer"))){
    lof_scores <- sapply(tune_k,function(k){dbscan::lof(scale(data),k = k)})
  } else {
    gower_dist <- cluster::daisy(new_data, metric = "gower",warnType = FALSE)
    lof_scores <- sapply(tune_k,function(k){dbscan::lof(gower_dist,k = k)})
  }
  # average over the different tuning parameters:
  aggregated_lofs <- rowMeans(lof_scores, na.rm = T)
  # quick visualization
  boxplot_scores <- cont_boxplot(aggregated_lofs, var_name = "LOF-scores",
                                 color = col1)
  return(list(
    scores = aggregated_lofs,
    boxplot = boxplot_scores
  ))
}

#' The function is a shortcut to extract the relevant rows of the input data of the lof_fun function.
#'
#' @param data The tibble or dataframe used in the call of lof_fun.
#' @param lof_list The output list of the lof_fun function.
#' @param threshold Greater scores than the threshold will be extracted.
#'
#' @return tibble or dataframe with the relevant rows from data + a column with the LOF-scores
#' @export
#'
#' @author Emanuel Sommer
extract_rows_score <- function(data,lof_list,threshold = 1){
  data <- na.omit(data)
  data$LOF_scores <- lof_list$scores
  extract_log <- lof_list$scores > threshold
  res <- dplyr::arrange(data[extract_log,],desc(LOF_scores))
  return(res)
}


#' Visualization for LOF on 2 variables.
#'
#' This function enables a quick visualization (scatterplot) of the LOF scores alongside the data. This is only implemented for 2 variables.
#'
#' @param data The data that was initially used for the \code{lof_fun} function. (only 2 named columns!)
#' @param lof_list The output list of the lof_fun function.
#' @param col_low The color for low LOF-scores. Must be given as character.
#' @param col_high The color for high LOF-scores. Must be given as character.
#'
#' @return A ggplot2 object (scatterplot)
#' @export
#'
#' @import ggplot2
#'
#' @author Emanuel Sommer
lof_vis <- function(data,lof_list,col_low = "deeppink",col_high = "darkviolet"){
  if(dim(data)[2] != 2){stop("Data has not the required dimensions.")}
  data <- na.omit(data)
  data$LOF_scores <- lof_list$scores
  data_names <- names(data)
  ggplot(data = data,aes_string(x = data_names[1], y = data_names[2], size = "LOF_scores", col = "LOF_scores")) +
    geom_point() +
    scale_color_gradient(low = col_low, high = col_high, name = "LOF score") +
    labs(size = "LOF score")+
    theme_bw()+
    theme(panel.border = element_blank(),
          axis.line = element_line(color = "black"))
}


