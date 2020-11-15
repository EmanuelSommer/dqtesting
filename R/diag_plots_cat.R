
#' A barplot showing either the absolute or relative appearances of the levels of the input factor. Moreover a visual categories check can be performed by supplying a character vector of allowed categories.
#'
#' @param x_factor A factor vector.
#' @param var_name The variable name to be displayed under the boxplot. Must be given as character.
#' @param categories A character vector of categories. Categories that are not included in this vector will be highlighted on the plot.
#' @param length_original If the factor vector was already filtered one can supply an original length to get accurate results in the relatvie version of the barplot.
#' @param color1 The primary filling color of the barplot. Must be given as character.
#' @param color2 The secondary filling color of the barplot indicating categories that were not included in the supplied ones.
#' @param alpha A numeric value between 0 and 1 to adjust the transparancy of the barplot.
#' @param ttl An optional title for the plot. Must be given as charcater.
#' @param rel Logical value indicating whether the count or the density should be shown on the y axis. (default: count)
#' @param flip_axis Logical value indicating whether categories should be shown on the x or y axis. (default: x axis) Especially for long category names one should consider the axis flip.
#'
#' @return a ggplot2 object
#' @export
#' @import tibble
#' @import ggplot2
#' @import dplyr
#'
#' @author Emanuel Sommer
#' @examples
#' bar_cat(factor(c(1:10, 4, 4, 2, 7, 7, 7, 7)))
bar_cat <- function(x_factor,
                    var_name = "",
                    categories = NULL,
                    length_original = NULL,
                    color1 = "darkviolet",
                    color2 = "yellow2",
                    alpha = 0.8,
                    ttl = "",
                    rel = FALSE,
                    flip_axis = FALSE) {
  if (is.null(length_original)) {
    length_original <- length(x_factor)
  }
  if (flip_axis) {
    if (rel) {
      if (is.null(categories)) {
        tibble(x = x_factor) %>%
          mutate(x = as.character(x)) %>%
          count(x) %>%
          rename(absolute = n) %>%
          mutate(relative = absolute / length_original) %>%
          arrange(desc(absolute), x) %>%
          ggplot(aes(x = factor(x), y = relative)) +
          geom_bar(stat = "identity", col = color1, fill = color1, alpha = alpha) +
          labs(y = "density", x = var_name, title = ttl) +
          coord_flip(ylim = c(0, 1)) +
          theme_bw() +
          theme(
            panel.border = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line()
          )
      } else {
        tibble(x = x_factor) %>%
          mutate(x = as.character(x)) %>%
          count(x) %>%
          rename(absolute = n) %>%
          mutate(
            relative = absolute / length_original,
            in_cat = if_else(x %in% categories, 0, 1),
            in_cat = as.factor(in_cat)
          ) %>%
          arrange(desc(absolute), x) %>%
          ggplot(aes(x = factor(x), y = relative, col = in_cat, fill = in_cat)) +
          geom_bar(stat = "identity", alpha = alpha) +
          scale_discrete_manual(
            aesthetics = c("colour", "fill"), values = c(color1, color2), name = "The category is one of the specified ones:",
            labels = c("Yes", "No")
          ) +
          labs(y = "density", x = var_name, title = ttl) +
          coord_flip(ylim = c(0, 1)) +
          theme_bw() +
          theme(
            panel.border = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(),
            legend.position = "bottom"
          )
      }
    } else {
      if (is.null(categories)) {
        tibble(x = x_factor) %>%
          mutate(x = as.character(x)) %>%
          count(x) %>%
          rename(absolute = n) %>%
          mutate(relative = absolute / length_original) %>%
          arrange(desc(absolute), x) %>%
          ggplot(aes(x = factor(x), y = absolute)) +
          geom_bar(stat = "identity", col = color1, fill = color1, alpha = alpha) +
          labs(y = "count", x = var_name, title = ttl) +
          coord_flip() +
          theme_bw() +
          theme(
            panel.border = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line()
          )
      } else {
        tibble(x = x_factor) %>%
          mutate(x = as.character(x)) %>%
          count(x) %>%
          rename(absolute = n) %>%
          mutate(
            relative = absolute / length_original,
            in_cat = if_else(x %in% categories, 0, 1),
            in_cat = as.factor(in_cat)
          ) %>%
          arrange(desc(absolute), x) %>%
          ggplot(aes(x = factor(x), y = absolute, col = in_cat, fill = in_cat)) +
          geom_bar(stat = "identity", alpha = alpha) +
          scale_discrete_manual(
            aesthetics = c("colour", "fill"), values = c(color1, color2), name = "The category is one of the specified ones:",
            labels = c("Yes", "No")
          ) +
          labs(y = "count", x = var_name, title = ttl) +
          coord_flip() +
          theme_bw() +
          theme(
            panel.border = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(),
            legend.position = "bottom"
          )
      }
    }
  } else {
    if (rel) {
      if (is.null(categories)) {
        tibble(x = x_factor) %>%
          mutate(x = as.character(x)) %>%
          count(x) %>%
          rename(absolute = n) %>%
          mutate(relative = absolute / length_original) %>%
          arrange(desc(absolute), x) %>%
          ggplot(aes(x = factor(x), y = relative)) +
          geom_bar(stat = "identity", col = color1, fill = color1, alpha = alpha) +
          coord_cartesian(ylim = c(0, 1)) +
          labs(y = "density", x = var_name, title = ttl) +
          theme_bw() +
          theme(
            panel.border = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line()
          )
      } else {
        tibble(x = x_factor) %>%
          mutate(x = as.character(x)) %>%
          count(x) %>%
          rename(absolute = n) %>%
          mutate(
            relative = absolute / length_original,
            in_cat = if_else(x %in% categories, 0, 1),
            in_cat = as.factor(in_cat)
          ) %>%
          arrange(desc(absolute), x) %>%
          ggplot(aes(x = factor(x), y = relative, col = in_cat, fill = in_cat)) +
          geom_bar(stat = "identity", alpha = alpha) +
          scale_discrete_manual(
            aesthetics = c("colour", "fill"), values = c(color1, color2), name = "The category is one of the specified ones:",
            labels = c("Yes", "No")
          ) +
          coord_cartesian(ylim = c(0, 1)) +
          labs(y = "density", x = var_name, title = ttl) +
          theme_bw() +
          theme(
            panel.border = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(),
            legend.position = "bottom"
          )
      }
    } else {
      if (is.null(categories)) {
        tibble(x = x_factor) %>%
          mutate(x = as.character(x)) %>%
          count(x) %>%
          rename(absolute = n) %>%
          mutate(relative = absolute / length_original) %>%
          arrange(desc(absolute), x) %>%
          ggplot(aes(x = factor(x), y = absolute)) +
          geom_bar(stat = "identity", col = color1, fill = color1, alpha = alpha) +
          labs(y = "count", x = var_name, title = ttl) +
          theme_bw() +
          theme(
            panel.border = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line()
          )
      } else {
        tibble(x = x_factor) %>%
          mutate(x = as.character(x)) %>%
          count(x) %>%
          rename(absolute = n) %>%
          mutate(
            relative = absolute / length_original,
            in_cat = if_else(x %in% categories, 0, 1),
            in_cat = as.factor(in_cat)
          ) %>%
          arrange(desc(absolute), x) %>%
          ggplot(aes(x = factor(x), y = absolute, col = in_cat, fill = in_cat)) +
          geom_bar(stat = "identity", alpha = alpha) +
          scale_discrete_manual(
            aesthetics = c("colour", "fill"), values = c(color1, color2), name = "The category is one of the specified ones:",
            labels = c("Yes", "No")
          ) +
          labs(y = "count", x = var_name, title = ttl) +
          theme_bw() +
          theme(
            panel.border = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(),
            legend.position = "bottom"
          )
      }
    }
  }
}
