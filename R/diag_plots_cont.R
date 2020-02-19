#' Generate a boxplot with jittered points for a single numeric vector.
#'
#' @param x A numeric vector.
#' @param var_name The variable name to be displayed under the boxplot. Must be given as character.
#' @param color The color of the boxplot. Must be given as character.
#' @param size The size of the lines of the boxplot. Must be given as a positive numeric value.
#' @param width The width of the boxplot. Must be given as a positive numeric value.
#' @param title The title of the plot. Must be given as character.
#'
#' @return a ggplot2 object
#' @export
#' @import tibble
#' @import ggplot2
#'
#' @author Emanuel Sommer
#'
#' @examples cont_boxplot(runif(100))
cont_boxplot <- function(x,
                         var_name = "",
                         color = "darkviolet",
                         size = 1,
                         width = 0.35,
                         title = ""){
  ggplot(tibble(x = x),aes(x=1,y=x))+
    geom_boxplot(fill = NA,col = color,size = size, na.rm = TRUE)+
    geom_jitter(alpha = 0.3,width = width, na.rm = TRUE)+
    labs(x="",y= var_name, title = title)+
    theme_bw()+
    theme(panel.border = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
}



#' Generate a histogram of a numeric input vector. Moreover range bounds can be included in the visualiation and a relative or absolute histogram can be selected
#'
#' @param x A numeric vector.
#' @param var_name The variable name to be displayed under the boxplot. Must be given as character.
#' @param color The filling color of the histogram. Must be given as character.
#' @param range_col The color of the range bounds. Must be given as character
#' @param bin_num The number of bins of the histogram. Must be given as a positive integer value.
#' @param ttl The title of the plot. Must be given as character.
#' @param alpha A numeric value between 0 and 1 to adjust the transparancy of the histogram.
#' @param range_min A numeric value setting the lower range bound of the data. If the actual data exceeds the bound the bound is going to visually appear on the plot.
#' @param range_max A numeric value setting the upper range bound of the data. If the actual data exceeds the bound the bound is going to visually appear on the plot.
#' @param rel Logical value indicating whether the count or the density should be shown on the y axis. (default: count)
#'
#' @return a ggplot2 object
#' @export
#' @import tibble
#' @import ggplot2
#'
#' @author Emanuel Sommer
#'
#' @examples  hist_cont(rchisq(100, df = 0.5), title = "Test-title", range_min = -4, range_max = 3)
hist_cont <- function(x,
                      var_name = "",
                      color = "darkviolet",
                      range_col = "black",
                      bin_num = 100,
                      ttl = "",
                      alpha = 0.8,
                      range_min = NULL,
                      range_max = NULL,
                      rel = FALSE){
  #check input
  if(!is.numeric(x)){"x must be numeric!"}
  if(!is.logical(rel)){stop("rel must be logical!")}
  if(!(is.null(range_max) | is.numeric(range_max))){stop("The ranges must be numeric if given.")}
  if(!(is.null(range_min) | is.numeric(range_min))){stop("The ranges must be numeric if given.")}
  # actual function
  if(rel){
    if((ifelse(is.null(range_min),-Inf,range_min) < min(x, na.rm = TRUE)) && (ifelse(is.null(range_max),Inf,range_max) > max(x, na.rm = TRUE))){
      ggplot(tibble(x = x),aes(x = x, y = ..density..))+
        geom_histogram(bins = bin_num,fill = color,alpha = alpha, na.rm = TRUE)+
        labs(y="density",
             x= var_name,
             title = ttl,
             caption = paste("Number of bins:",bin_num))+
        theme_bw()+
        theme(legend.position="bottom",
              panel.border = element_blank(),
              axis.line = element_line(color = "black"))
    } else if((!(ifelse(is.null(range_min),-Inf,range_min) < min(x, na.rm = TRUE))) && (ifelse(is.null(range_max),Inf,range_max) > max(x, na.rm = TRUE))){
      ggplot(tibble(x = x),aes(x = x, y = ..density..))+
        geom_histogram(bins = bin_num,fill = color,alpha = alpha, na.rm = TRUE)+
        geom_vline(xintercept = range_min, linetype = "longdash", col = range_col)+
        labs(y="density",
             x= var_name,
             title = ttl,
             subtitle = "The vertical line represents the lower range bound.",
             caption = paste("Number of bins:",bin_num))+
        theme_bw()+
        theme(legend.position="bottom",
              panel.border = element_blank(),
              axis.line = element_line(color = "black"))
    } else if((ifelse(is.null(range_min),-Inf,range_min) < min(x, na.rm = TRUE)) && (!(ifelse(is.null(range_max),Inf,range_max) > max(x, na.rm = TRUE)))){
      ggplot(tibble(x = x),aes(x = x, y = ..density..))+
        geom_histogram(bins = bin_num,fill = color,alpha = alpha, na.rm = TRUE)+
        geom_vline(xintercept = range_max, linetype = "longdash", col = range_col)+
        labs(y="density",
             x= var_name,
             title = ttl,
             subtitle = "The vertical line represents the upper range bound.",
             caption = paste("Number of bins:",bin_num))+
        theme_bw()+
        theme(legend.position="bottom",
              panel.border = element_blank(),
              axis.line = element_line(color = "black"))
    } else if((!(ifelse(is.null(range_min),-Inf,range_min) < min(x, na.rm = TRUE))) && (!(ifelse(is.null(range_max),Inf,range_max) > max(x, na.rm = TRUE)))){
      ggplot(tibble(x = x),aes(x = x, y = ..density..))+
        geom_histogram(bins = bin_num,fill = color,alpha = alpha, na.rm = TRUE)+
        geom_vline(xintercept = range_max, linetype = "longdash", col = range_col)+
        geom_vline(xintercept = range_min, linetype = "longdash", col = range_col)+
        labs(y="density",
             x= var_name,
             title = ttl,
             subtitle = "The vertical lines represent the lower and upper range bound.",
             caption = paste("Number of bins:",bin_num))+
        theme_bw()+
        theme(legend.position="bottom",
              panel.border = element_blank(),
              axis.line = element_line(color = "black"))
    }
  } else {
    if((ifelse(is.null(range_min),-Inf,range_min) < min(x, na.rm = TRUE)) && (ifelse(is.null(range_max),Inf,range_max) > max(x, na.rm = TRUE))){
       ggplot(tibble(x = x),aes(x = x))+
          geom_histogram(bins = bin_num,fill = color,alpha = alpha, na.rm = TRUE)+
          labs(y="count",
               x= var_name,
               title = ttl,
               caption = paste("Number of bins:",bin_num))+
          theme_bw()+
          theme(legend.position="bottom",
                panel.border = element_blank(),
                axis.line = element_line(color = "black"))
    } else if((!(ifelse(is.null(range_min),-Inf,range_min) < min(x, na.rm = TRUE))) && (ifelse(is.null(range_max),Inf,range_max) > max(x, na.rm = TRUE))){
      ggplot(tibble(x = x),aes(x = x))+
        geom_histogram(bins = bin_num,fill = color,alpha = alpha, na.rm = TRUE)+
        geom_vline(xintercept = range_min, linetype = "longdash", col = range_col)+
        labs(y="count",
             x= var_name,
             title = ttl,
             subtitle = "The vertical line represents the lower range bound.",
             caption = paste("Number of bins:",bin_num))+
        theme_bw()+
        theme(legend.position="bottom",
              panel.border = element_blank(),
              axis.line = element_line(color = "black"))
    } else if((ifelse(is.null(range_min),-Inf,range_min) < min(x, na.rm = TRUE)) && (!(ifelse(is.null(range_max),Inf,range_max) > max(x, na.rm = TRUE)))){
      ggplot(tibble(x = x),aes(x = x))+
        geom_histogram(bins = bin_num,fill = color,alpha = alpha, na.rm = TRUE)+
        geom_vline(xintercept = range_max, linetype = "longdash", col = range_col)+
        labs(y="count",
             x= var_name,
             title = ttl,
             subtitle = "The vertical line represents the upper range bound.",
             caption = paste("Number of bins:",bin_num))+
        theme_bw()+
        theme(legend.position="bottom",
              panel.border = element_blank(),
              axis.line = element_line(color = "black"))
    } else if((!(ifelse(is.null(range_min),-Inf,range_min) < min(x, na.rm = TRUE))) && (!(ifelse(is.null(range_max),Inf,range_max) > max(x, na.rm = TRUE)))){
      ggplot(tibble(x = x),aes(x = x))+
        geom_histogram(bins = bin_num,fill = color,alpha = alpha, na.rm = TRUE)+
        geom_vline(xintercept = range_max, linetype = "longdash", col = range_col)+
        geom_vline(xintercept = range_min, linetype = "longdash", col = range_col)+
        labs(y="count",
             x= var_name,
             title = ttl,
             subtitle = "The vertical lines represent the lower and upper range bound.",
             caption = paste("Number of bins:",bin_num))+
        theme_bw()+
        theme(legend.position="bottom",
              panel.border = element_blank(),
              axis.line = element_line(color = "black"))
    }
  }
}





