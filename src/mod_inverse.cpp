/*
Uses the Extended Euclidian Approach
Accounts for negative base values by
Performing a mod operation on it first
Will exit if no modular inverse exists
Incase base and mod are not co-prime
*/

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int mod_inv(int base, int mod) {
  base = base % mod;
  int a = base;
  int b = mod;
  int larger = std::max(a, b);
  int smaller = std::min(a, b);
  int t1 = 0;
  int t2 = 1;

  while (smaller != 0) {
    int q = larger / smaller;
    int r = larger % smaller;
    int t = t1 - t2 * q;
    larger = smaller;
    smaller = r;
    t1 = t2;
    t2 = t;
  }

  if (larger != 1) {
    Rcpp::stop("No modular inverse exists");
  }

  if (t1 < 0) {
    t1 += mod;
  }
  return t1;
}
