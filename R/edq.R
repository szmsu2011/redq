edqd_mat <- function(x, p) {
  etqd_ls <- array_branch(x, 1) |>
    future_map(etqd_vec, p = p, .options = furrr_options(seed = NULL))
  colSums(inject(rbind(!!!etqd_ls)))
}

#' Compute empirical dynamic quantiles for a temporal data set
#'
#' @param x The data.
#' @param p A numeric vector of probabilities with values in `[0,1]`.
#' @param ... <[`tidy-select`][dplyr_tidy_select]> If a `data.frame` is passed
#'   to `x`, the measured time series variables in the data set.
#' @param n_core Number of cores used for parallel processing. See
#'   [parallelly::supportsMulticore()] for its availability.
#'
#' @return The EDQ data.
#' @rdname edq
#'
#' @references
#' Daniel Peña, Ruey S. Tsay & Ruben Zamar (2019) Empirical Dynamic Quantiles
#' for Visualization of High-Dimensional Time Series, Technometrics, 61:4,
#' 429-444, DOI: 10.1080/00401706.2019.1575285
#'
#' @examples
#' library(redq)
#' library(purrr)
#' library(tidyr)
#' library(dplyr)
#' library(ggplot2)
#'
#' set.seed(2023)
#' arima_data <- map(seq_len(500), \(x) {
#'   arima.sim(list(ar = c(.95, -.1)), 100, mean = x / 500 - .5)
#' }) |>
#'   set_names(sprintf("ts%s", seq_len(500))) |>
#'   as_tibble()
#'
#' edq_data <- edq(arima_data, c(.25, .5, .75), n_core = 1L)
#'
#' arima_data |>
#'   mutate(time = seq_len(nrow(arima_data))) |>
#'   pivot_longer(starts_with("ts")) |>
#'   ggplot(aes(time, value, group = name)) +
#'   geom_line() +
#'   geom_line(
#'     aes(col = name),
#'     data = edq_data |>
#'       mutate(time = seq_len(nrow(arima_data))) |>
#'       pivot_longer(starts_with("q_"))
#'   ) +
#'   theme_bw() +
#'   theme(legend.title = element_blank()) +
#'   labs(y = "")
#' @export
edq <- function(x, p, ..., n_core = 1L) {
  p <- suppressWarnings(as.numeric(p))
  if (!isTRUE(try(all(p >= 0 & p <= 1), silent = TRUE))) {
    abort("`p` must be coercible to `numeric` and in [0,1].")
  }
  UseMethod("edq")
}

#' @export
edq.matrix <- function(x, p, ..., n_core = 1L) {
  if (any(is.na(x))) {
    abort("Missing values detected, please inspect the data.")
  }
  if (n_core > 1L) {
    if (!supportsMulticore()) {
      abort(paste(
        "Process forking not supported in current OS or R environment.",
        "Please retry with `n_core = 1L` or avoid MS Windows and RStudio."
      ))
    }
    if (n_core > availableCores()) {
      n_core <- availableCores()
      warn(sprintf("Setting `n_core = %sL`.", n_core))
    }
    plan(multicore, workers = n_core)
  } else {
    plan(sequential)
  }
  edq_tbl <- x[, map_dbl(p, \(p) which.min(edqd_mat(x, p)))] |>
    as.matrix() |>
    array_branch(2) |>
    set_names(sprintf("q_%s", round(p, 3))) |>
    as_tibble()
  plan(sequential)
  edq_tbl
}

#' @export
edq.data.frame <- function(x, p, ..., n_core = 1L) {
  if (length(dots_list(...))) x <- select(x, ...)
  edq(as.matrix(select(x, where(is.numeric))), p, n_core = n_core)
}

#' @export
edq.tbl_ts <- function(x, p, ..., n_core = 1L) {
  abort("edq for tsibble is not ready.")
}
