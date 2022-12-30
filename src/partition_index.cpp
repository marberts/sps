#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector partition_index(NumericVector x, int n, bool decreasing) {
  IntegerVector y = seq_along(x);
  if (decreasing) {
    std::nth_element(
      y.begin(), y.begin() + n, y.end(),
      [x](int i, int j) {return x[i - 1] > x[j - 1];}
    );
  } else {
    std::nth_element(
      y.begin(), y.begin() + n, y.end(),
      [x](int i, int j) {return x[i - 1] < x[j - 1];}
    );
  }
  return y;
}