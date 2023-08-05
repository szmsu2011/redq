#' redq: fast algorithm for computing empirical dynamic quantiles for time series
#' @importFrom rlang dots_list set_names abort warn inject
#' @importFrom dplyr select where
#' @importFrom tibble as_tibble
#' @importFrom purrr array_branch map_dbl
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan multicore availableCores sequential
#' @importFrom parallelly supportsMulticore
#' @importFrom Rcpp sourceCpp
#' @useDynLib redq, .registration = TRUE
"_PACKAGE"
