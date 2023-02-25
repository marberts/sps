#include "cpp11.hpp"
#include <numeric>
#include <vector>
using namespace cpp11;

[[cpp11::register]]

std::vector<int> partition_index(doubles x, int n, bool decreasing) {
  std::vector<int> res(x.size());
  std::iota(res.begin(), res.end(), 1);
  std::nth_element(
    res.begin(), res.begin() + n, res.end(),
    [x, decreasing](int i, int j) {
      return decreasing ? x[i - 1] > x[j - 1] : x[i - 1] < x[j - 1];
    }
  );
  return res;
}