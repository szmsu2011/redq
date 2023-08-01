library(purrr)
library(furrr)
library(rlang)
library(tibble)
library(dplyr)

etqd_vec <- function(x, p) {
  P <- p + outer(x, x, "<") * (1 - p * 2)
  D <- abs(outer(x, x, "-"))
  map_dbl(seq_len(nrow(P)), \(i) sum(P[, i] * D[i, ]))
}

edqd_mat <- function(x, p, n_sessions = 1L) {
  op <- plan(multisession, workers = n_sessions)
  etqd_ls <- future_map(array_branch(x, 1), etqd_vec, p = p)
  plan(op)
  colSums(inject(rbind(!!!etqd_ls)))
}

edq <- function(x, p, ...) {
  p <- suppressWarnings(as.numeric(p))
  if (!isTRUE(try(all(p >= 0 & p <= 1), silent = TRUE))) {
    abort("`p` must be coercible to `numeric` and in [0,1].")
  }
  UseMethod("edq")
}

edq.matrix <- function(x, p, ...) {
  x[, map_dbl(p, \(p) which.min(edqd_mat(x, p)))] |>
    as.matrix() |>
    array_branch(2) |>
    set_names(sprintf("q_%s", round(p, 3))) |>
    as_tibble()
}

edq.data.frame <- function(x, p, ts_col = seq_len(ncol(x))) {
  edq(as.matrix(select(x[, ts_col], where(is.numeric))), p)
}

edq.tbl_ts <- function(...) {
  abort("edq for tsibble is not ready.")
}

## Examples
library(tidyr)
library(ggplot2)

set.seed(2023)

arima_data <- map(seq_len(500), \(x) {
  arima.sim(list(ar = c(.95, -.1)), 100, mean = x / 500 - .5)
}) |>
  set_names(sprintf("ts%s", seq_len(500))) |>
  as_tibble()

edq_data <- edq(arima_data, c(.25, .5, .75))

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
