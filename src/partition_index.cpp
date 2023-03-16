#include "cpp11.hpp"
#include <numeric>
#include <vector>

using namespace cpp11;
using namespace std;

[[cpp11::register]]

std::vector<int> partition_index(doubles x, int n, bool decreasing) {
  vector<int> res(x.size());
  iota(res.begin(), res.end(), 1);
  nth_element(
    res.begin(), res.begin() + n - 1, res.end(),
    [x, decreasing](int i, int j) {
      return decreasing ? x[i - 1] > x[j - 1] : x[i - 1] < x[j - 1];
    }
  );
  return res;
}
