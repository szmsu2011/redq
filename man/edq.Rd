% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edq.R
\name{edq}
\alias{edq}
\title{Compute empirical dynamic quantiles for a temporal data set}
\usage{
edq(x, p, ..., n_core = 1L)
}
\arguments{
\item{x}{The data.}

\item{p}{A numeric vector of probabilities with values in \verb{[0,1]}.}

\item{...}{<\code{\link[=dplyr_tidy_select]{tidy-select}}> If a \code{data.frame} is passed
to \code{x}, the measured time series variables in the data set.}

\item{n_core}{Number of cores used for parallel processing. See
\code{\link[parallelly:supportsMulticore]{parallelly::supportsMulticore()}} for its availability.}
}
\value{
The EDQ data.
}
\description{
Compute empirical dynamic quantiles for a temporal data set
}
\examples{
library(redq)
library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)

set.seed(2023)
arima_data <- map(seq_len(500), \(x) {
  arima.sim(list(ar = c(.95, -.1)), 100, mean = x / 500 - .5)
}) |>
  set_names(sprintf("ts\%s", seq_len(500))) |>
  as_tibble()

edq_data <- edq(arima_data, c(.25, .5, .75), n_core = 1L)

arima_data |>
  mutate(time = seq_len(nrow(arima_data))) |>
  pivot_longer(starts_with("ts")) |>
  ggplot(aes(time, value, group = name)) +
  geom_line() +
  geom_line(
    aes(col = name),
    data = edq_data |>
      mutate(time = seq_len(nrow(arima_data))) |>
      pivot_longer(starts_with("q_"))
  ) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  labs(y = "")
}
\references{
Daniel Peña, Ruey S. Tsay & Ruben Zamar (2019) Empirical Dynamic Quantiles
for Visualization of High-Dimensional Time Series, Technometrics, 61:4,
429-444, DOI: 10.1080/00401706.2019.1575285
}
