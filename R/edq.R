etqd_vec <- function(x, p) {
  P <- p + outer(x, x, "<") * (1 - p * 2)
  D <- abs(outer(x, x, "-"))
  map_dbl(seq_len(nrow(P)), \(i) sum(P[, i] * D[i, ], na.rm = TRUE))
}

edqd_mat <- function(x, p, n_sessions) {
  op <- plan(multisession, workers = n_sessions)
  etqd_ls <- future_map(array_branch(x, 1), etqd_vec, p = p)
  plan(op)
  colSums(inject(rbind(!!!etqd_ls)))
}

#' Compute empirical dynamic quantiles for a temporal data set
#'
#' @param x The data.
#' @param p A numeric vector of probabilities with values in `[0,1]`.
#' @param ... <[`tidy-select`][dplyr_tidy_select]> If a `data.frame` is passed
#'   to `x`, the measured time series variables in the data set.
#' @param n_sessions Number of sessions for parallel processing.
#'
#' @return The EDQ data.
#' @rdname edq
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
#' edq_data <- edq(arima_data, c(.25, .5, .75))
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
edq <- function(x, p, ..., n_sessions = 1L) {
  p <- suppressWarnings(as.numeric(p))
  if (!isTRUE(try(all(p >= 0 & p <= 1), silent = TRUE))) {
    abort("`p` must be coercible to `numeric` and in [0,1].")
  }
  UseMethod("edq")
}

#' @export
edq.matrix <- function(x, p, ..., n_sessions = 1L) {
  if (any(is.na(x))) {
    warn("Missing values detected, the results might be biased.")
  }
  x[, map_dbl(p, \(p) which.min(edqd_mat(x, p, n_sessions)))] |>
    as.matrix() |>
    array_branch(2) |>
    set_names(sprintf("q_%s", round(p, 3))) |>
    as_tibble()
}

#' @export
edq.data.frame <- function(x, p, ..., n_sessions = 1L) {
  if (length(dots_list(...))) x <- select(x, ...)
  edq(as.matrix(select(x, where(is.numeric))), p, n_sessions = n_sessions)
}

#' @export
edq.tbl_ts <- function(x, p, ..., n_sessions = 1L) {
  abort("edq for tsibble is not ready.")
}
