% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diag_plots_cont.R
\name{hist_cont}
\alias{hist_cont}
\title{Generate a histogram of a numeric input vector. Moreover range bounds can be included in the visualiation and a relative or absolute histogram can be selected}
\usage{
hist_cont(
  x,
  var_name = "",
  color = "darkviolet",
  range_col = "black",
  bin_num = 100,
  ttl = "",
  alpha = 0.8,
  range_min = NULL,
  range_max = NULL,
  rel = FALSE
)
}
\arguments{
\item{x}{A numeric vector.}

\item{var_name}{The variable name to be displayed under the boxplot. Must be given as character.}

\item{color}{The filling color of the histogram. Must be given as character.}

\item{range_col}{The color of the range bounds. Must be given as character}

\item{bin_num}{The number of bins of the histogram. Must be given as a positive integer value.}

\item{ttl}{The title of the plot. Must be given as character.}

\item{alpha}{A numeric value between 0 and 1 to adjust the transparancy of the histogram.}

\item{range_min}{A numeric value setting the lower range bound of the data. If the actual data exceeds the bound the bound is going to visually appear on the plot.}

\item{range_max}{A numeric value setting the upper range bound of the data. If the actual data exceeds the bound the bound is going to visually appear on the plot.}

\item{rel}{Logical value indicating whether the count or the density should be shown on the y axis. (default: count)}
}
\value{
a ggplot2 object
}
\description{
Generate a histogram of a numeric input vector. Moreover range bounds can be included in the visualiation and a relative or absolute histogram can be selected
}
\examples{
 hist_cont(rchisq(100, df = 0.5), ttl = "Test-title", range_min = -4, range_max = 3)
}
\author{
Emanuel Sommer
}
