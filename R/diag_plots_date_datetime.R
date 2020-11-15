#' Generate a histogram of a date input vector. Moreover range bounds can be included in the visualiation and a relative or absolute histogram can be selected
#'
#' @param x A date vector.
#' @param var_name The variable name to be displayed under the boxplot. Must be given as character.
#' @param color The filling color of the histogram. Must be given as character.
#' @param range_col The color of the range bounds. Must be given as character
#' @param bin_num The number of bins of the histogram. Must be given as a positive integer value.
#' @param ttl The title of the plot. Must be given as character.
#' @param alpha A numeric value between 0 and 1 to adjust the transparancy of the histogram.
#' @param range_min A date value setting the lower range bound of the data. If the actual data exceeds the bound the bound is going to visually appear on the plot.
#' @param range_max A date value setting the upper range bound of the data. If the actual data exceeds the bound the bound is going to visually appear on the plot.
#' @param rel Logical value indicating whether the count or the density should be shown on the y axis. (default: count)
#'
#' @return a ggplot2 object
#' @export
#' @import tibble
#' @import ggplot2
#'
#' @author Emanuel Sommer
hist_date <- function(x,
                      var_name = "",
                      color = "darkviolet",
                      range_col = "black",
                      bin_num = 100,
                      ttl = "",
                      alpha = 0.8,
                      range_min = NULL,
                      range_max = NULL,
                      rel = FALSE) {
  # check input
  if (!lubridate::is.Date(x)) {
    stop("x must be Date!")
  }
  if (!is.logical(rel)) {
    stop("rel must be logical!")
  }
  if (!(is.null(range_max) | lubridate::is.Date(range_max))) {
    stop("The ranges must be Date if given.")
  }
  if (!(is.null(range_min) | lubridate::is.Date(range_min))) {
    stop("The ranges must be Date if given.")
  }
  # actual function
  if (rel) {
    if ((ifelse(is.null(range_min), as.Date("0000-01-01"), range_min) < min(x, na.rm = TRUE)) && (ifelse(is.null(range_max), as.Date("9999-12-31"), range_max) > max(x, na.rm = TRUE))) {
      ggplot(tibble(x = x), aes(x = x, y = ..density..)) +
        geom_histogram(bins = bin_num, fill = color, alpha = alpha, na.rm = TRUE) +
        labs(
          y = "density",
          x = var_name,
          title = ttl,
          caption = paste("Number of bins:", bin_num)
        ) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.line = element_line(color = "black")
        )
    } else if ((!(ifelse(is.null(range_min), as.Date("0000-01-01"), range_min) < min(x, na.rm = TRUE))) && (ifelse(is.null(range_max), as.Date("9999-12-31"), range_max) > max(x, na.rm = TRUE))) {
      ggplot(tibble(x = x), aes(x = x, y = ..density..)) +
        geom_histogram(bins = bin_num, fill = color, alpha = alpha, na.rm = TRUE) +
        geom_vline(xintercept = range_min, linetype = "longdash", col = range_col) +
        labs(
          y = "density",
          x = var_name,
          title = ttl,
          subtitle = "The vertical line represents the lower range bound.",
          caption = paste("Number of bins:", bin_num)
        ) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.line = element_line(color = "black")
        )
    } else if ((ifelse(is.null(range_min), as.Date("0000-01-01"), range_min) < min(x, na.rm = TRUE)) && (!(ifelse(is.null(range_max), as.Date("9999-12-31"), range_max) > max(x, na.rm = TRUE)))) {
      ggplot(tibble(x = x), aes(x = x, y = ..density..)) +
        geom_histogram(bins = bin_num, fill = color, alpha = alpha, na.rm = TRUE) +
        geom_vline(xintercept = range_max, linetype = "longdash", col = range_col) +
        labs(
          y = "density",
          x = var_name,
          title = ttl,
          subtitle = "The vertical line represents the upper range bound.",
          caption = paste("Number of bins:", bin_num)
        ) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.line = element_line(color = "black")
        )
    } else if ((!(ifelse(is.null(range_min), as.Date("0000-01-01"), range_min) < min(x, na.rm = TRUE))) && (!(ifelse(is.null(range_max), as.Date("9999-12-31"), range_max) > max(x, na.rm = TRUE)))) {
      ggplot(tibble(x = x), aes(x = x, y = ..density..)) +
        geom_histogram(bins = bin_num, fill = color, alpha = alpha, na.rm = TRUE) +
        geom_vline(xintercept = range_max, linetype = "longdash", col = range_col) +
        geom_vline(xintercept = range_min, linetype = "longdash", col = range_col) +
        labs(
          y = "density",
          x = var_name,
          title = ttl,
          subtitle = "The vertical lines represent the lower and upper range bound.",
          caption = paste("Number of bins:", bin_num)
        ) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.line = element_line(color = "black")
        )
    }
  } else {
    if ((ifelse(is.null(range_min), as.Date("0000-01-01"), range_min) < min(x, na.rm = TRUE)) && (ifelse(is.null(range_max), as.Date("9999-12-31"), range_max) > max(x, na.rm = TRUE))) {
      ggplot(tibble(x = x), aes(x = x)) +
        geom_histogram(bins = bin_num, fill = color, alpha = alpha, na.rm = TRUE) +
        labs(
          y = "count",
          x = var_name,
          title = ttl,
          caption = paste("Number of bins:", bin_num)
        ) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.line = element_line(color = "black")
        )
    } else if ((!(ifelse(is.null(range_min), as.Date("0000-01-01"), range_min) < min(x, na.rm = TRUE))) && (ifelse(is.null(range_max), as.Date("9999-12-31"), range_max) > max(x, na.rm = TRUE))) {
      ggplot(tibble(x = x), aes(x = x)) +
        geom_histogram(bins = bin_num, fill = color, alpha = alpha, na.rm = TRUE) +
        geom_vline(xintercept = range_min, linetype = "longdash", col = range_col) +
        labs(
          y = "count",
          x = var_name,
          title = ttl,
          subtitle = "The vertical line represents the lower range bound.",
          caption = paste("Number of bins:", bin_num)
        ) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.line = element_line(color = "black")
        )
    } else if ((ifelse(is.null(range_min), as.Date("0000-01-01"), range_min) < min(x, na.rm = TRUE)) && (!(ifelse(is.null(range_max), as.Date("9999-12-31"), range_max) > max(x, na.rm = TRUE)))) {
      ggplot(tibble(x = x), aes(x = x)) +
        geom_histogram(bins = bin_num, fill = color, alpha = alpha, na.rm = TRUE) +
        geom_vline(xintercept = range_max, linetype = "longdash", col = range_col) +
        labs(
          y = "count",
          x = var_name,
          title = ttl,
          subtitle = "The vertical line represents the upper range bound.",
          caption = paste("Number of bins:", bin_num)
        ) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.line = element_line(color = "black")
        )
    } else if ((!(ifelse(is.null(range_min), as.Date("0000-01-01"), range_min) < min(x, na.rm = TRUE))) && (!(ifelse(is.null(range_max), as.Date("9999-12-31"), range_max) > max(x, na.rm = TRUE)))) {
      ggplot(tibble(x = x), aes(x = x)) +
        geom_histogram(bins = bin_num, fill = color, alpha = alpha, na.rm = TRUE) +
        geom_vline(xintercept = range_max, linetype = "longdash", col = range_col) +
        geom_vline(xintercept = range_min, linetype = "longdash", col = range_col) +
        labs(
          y = "count",
          x = var_name,
          title = ttl,
          subtitle = "The vertical lines represent the lower and upper range bound.",
          caption = paste("Number of bins:", bin_num)
        ) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.line = element_line(color = "black")
        )
    }
  }
}


#' Generate a histogram of a datetime input vector. Moreover range bounds can be included in the visualiation and a relative or absolute histogram can be selected
#'
#' @param x A datetime vector (POSIXct).
#' @param var_name The variable name to be displayed under the boxplot. Must be given as character.
#' @param color The filling color of the histogram. Must be given as character.
#' @param range_col The color of the range bounds. Must be given as character
#' @param bin_num The number of bins of the histogram. Must be given as a positive integer value.
#' @param ttl The title of the plot. Must be given as character.
#' @param alpha A numeric value between 0 and 1 to adjust the transparancy of the histogram.
#' @param range_min A POSIXct value setting the lower range bound of the data. If the actual data exceeds the bound the bound is going to visually appear on the plot.
#' @param range_max A POSIXct value setting the upper range bound of the data. If the actual data exceeds the bound the bound is going to visually appear on the plot.
#' @param rel Logical value indicating whether the count or the density should be shown on the y axis. (default: count)
#'
#' @return a ggplot2 object
#' @export
#' @import tibble
#' @import ggplot2
#'
#' @author Emanuel Sommer
hist_datetime <- function(x,
                          var_name = "",
                          color = "darkviolet",
                          range_col = "black",
                          bin_num = 100,
                          ttl = "",
                          alpha = 0.8,
                          range_min = NULL,
                          range_max = NULL,
                          rel = FALSE) {
  # check input
  if (!lubridate::is.POSIXct(x)) {
    stop("x must be POSIXct!")
  }
  if (!is.logical(rel)) {
    stop("rel must be logical!")
  }
  if (!(is.null(range_max) | lubridate::is.POSIXct(range_max))) {
    stop("The ranges must be POSIXct if given.")
  }
  if (!(is.null(range_min) | lubridate::is.POSIXct(range_min))) {
    stop("The ranges must be POSIXct if given.")
  }
  # actual function
  if (rel) {
    if ((ifelse(is.null(range_min), as.POSIXct("0000-01-01 00:00:00"), range_min) < min(x, na.rm = TRUE)) && (ifelse(is.null(range_max), as.POSIXct("9999-12-31 23:59:59"), range_max) > max(x, na.rm = TRUE))) {
      ggplot(tibble(x = x), aes(x = x, y = ..density..)) +
        geom_histogram(bins = bin_num, fill = color, alpha = alpha, na.rm = TRUE) +
        labs(
          y = "density",
          x = var_name,
          title = ttl,
          caption = paste("Number of bins:", bin_num)
        ) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.line = element_line(color = "black")
        )
    } else if ((!(ifelse(is.null(range_min), as.POSIXct("0000-01-01 00:00:00"), range_min) < min(x, na.rm = TRUE))) && (ifelse(is.null(range_max), as.POSIXct("9999-12-31 23:59:59"), range_max) > max(x, na.rm = TRUE))) {
      ggplot(tibble(x = x), aes(x = x, y = ..density..)) +
        geom_histogram(bins = bin_num, fill = color, alpha = alpha, na.rm = TRUE) +
        geom_vline(xintercept = range_min, linetype = "longdash", col = range_col) +
        labs(
          y = "density",
          x = var_name,
          title = ttl,
          subtitle = "The vertical line represents the lower range bound.",
          caption = paste("Number of bins:", bin_num)
        ) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.line = element_line(color = "black")
        )
    } else if ((ifelse(is.null(range_min), as.POSIXct("0000-01-01 00:00:00"), range_min) < min(x, na.rm = TRUE)) && (!(ifelse(is.null(range_max), as.POSIXct("9999-12-31 23:59:59"), range_max) > max(x, na.rm = TRUE)))) {
      ggplot(tibble(x = x), aes(x = x, y = ..density..)) +
        geom_histogram(bins = bin_num, fill = color, alpha = alpha, na.rm = TRUE) +
        geom_vline(xintercept = range_max, linetype = "longdash", col = range_col) +
        labs(
          y = "density",
          x = var_name,
          title = ttl,
          subtitle = "The vertical line represents the upper range bound.",
          caption = paste("Number of bins:", bin_num)
        ) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.line = element_line(color = "black")
        )
    } else if ((!(ifelse(is.null(range_min), as.POSIXct("0000-01-01 00:00:00"), range_min) < min(x, na.rm = TRUE))) && (!(ifelse(is.null(range_max), as.POSIXct("9999-12-31 23:59:59"), range_max) > max(x, na.rm = TRUE)))) {
      ggplot(tibble(x = x), aes(x = x, y = ..density..)) +
        geom_histogram(bins = bin_num, fill = color, alpha = alpha, na.rm = TRUE) +
        geom_vline(xintercept = range_max, linetype = "longdash", col = range_col) +
        geom_vline(xintercept = range_min, linetype = "longdash", col = range_col) +
        labs(
          y = "density",
          x = var_name,
          title = ttl,
          subtitle = "The vertical lines represent the lower and upper range bound.",
          caption = paste("Number of bins:", bin_num)
        ) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.line = element_line(color = "black")
        )
    }
  } else {
    if ((ifelse(is.null(range_min), as.POSIXct("0000-01-01 00:00:00"), range_min) < min(x, na.rm = TRUE)) && (ifelse(is.null(range_max), as.POSIXct("9999-12-31 23:59:59"), range_max) > max(x, na.rm = TRUE))) {
      ggplot(tibble(x = x), aes(x = x)) +
        geom_histogram(bins = bin_num, fill = color, alpha = alpha, na.rm = TRUE) +
        labs(
          y = "count",
          x = var_name,
          title = ttl,
          caption = paste("Number of bins:", bin_num)
        ) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.line = element_line(color = "black")
        )
    } else if ((!(ifelse(is.null(range_min), as.POSIXct("0000-01-01 00:00:00"), range_min) < min(x, na.rm = TRUE))) && (ifelse(is.null(range_max), as.POSIXct("9999-12-31 23:59:59"), range_max) > max(x, na.rm = TRUE))) {
      ggplot(tibble(x = x), aes(x = x)) +
        geom_histogram(bins = bin_num, fill = color, alpha = alpha, na.rm = TRUE) +
        geom_vline(xintercept = range_min, linetype = "longdash", col = range_col) +
        labs(
          y = "count",
          x = var_name,
          title = ttl,
          subtitle = "The vertical line represents the lower range bound.",
          caption = paste("Number of bins:", bin_num)
        ) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.line = element_line(color = "black")
        )
    } else if ((ifelse(is.null(range_min), as.POSIXct("0000-01-01 00:00:00"), range_min) < min(x, na.rm = TRUE)) && (!(ifelse(is.null(range_max), as.POSIXct("9999-12-31 23:59:59"), range_max) > max(x, na.rm = TRUE)))) {
      ggplot(tibble(x = x), aes(x = x)) +
        geom_histogram(bins = bin_num, fill = color, alpha = alpha, na.rm = TRUE) +
        geom_vline(xintercept = range_max, linetype = "longdash", col = range_col) +
        labs(
          y = "count",
          x = var_name,
          title = ttl,
          subtitle = "The vertical line represents the upper range bound.",
          caption = paste("Number of bins:", bin_num)
        ) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.line = element_line(color = "black")
        )
    } else if ((!(ifelse(is.null(range_min), as.POSIXct("0000-01-01 00:00:00"), range_min) < min(x, na.rm = TRUE))) && (!(ifelse(is.null(range_max), as.POSIXct("9999-12-31 23:59:59"), range_max) > max(x, na.rm = TRUE)))) {
      ggplot(tibble(x = x), aes(x = x)) +
        geom_histogram(bins = bin_num, fill = color, alpha = alpha, na.rm = TRUE) +
        geom_vline(xintercept = range_max, linetype = "longdash", col = range_col) +
        geom_vline(xintercept = range_min, linetype = "longdash", col = range_col) +
        labs(
          y = "count",
          x = var_name,
          title = ttl,
          subtitle = "The vertical lines represent the lower and upper range bound.",
          caption = paste("Number of bins:", bin_num)
        ) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.line = element_line(color = "black")
        )
    }
  }
}
