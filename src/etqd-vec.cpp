#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector etqd_vec(NumericVector x, double p) {
  int n = x.size();
  NumericMatrix P(n, n);
  NumericMatrix D(n, n);

  for(int i = 0; i < n; ++i) {
    for(int j = 0; j < n; ++j) {
      P(i, j) = p + (x[i] < x[j] ? 1 : 0) * (1 - p * 2);
      D(i, j) = std::abs(x[i] - x[j]);
    }
  }
  NumericVector etqd(n);
  for(int i = 0; i < n; ++i) {
    double sum = 0;
    for(int j = 0; j < n; ++j) {
      sum += P(j, i) * D(i, j);
    }
    etqd[i] = sum;
  }

  return etqd;
}
