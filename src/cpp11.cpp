// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// partition_index.cpp
 std::vector<int> partition_index(doubles x, int n, bool decreasing);
extern "C" SEXP _sps_partition_index(SEXP x, SEXP n, SEXP decreasing) {
  BEGIN_CPP11
    return cpp11::as_sexp(partition_index(cpp11::as_cpp<cpp11::decay_t<doubles>>(x), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<bool>>(decreasing)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_sps_partition_index", (DL_FUNC) &_sps_partition_index, 3},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_sps(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
