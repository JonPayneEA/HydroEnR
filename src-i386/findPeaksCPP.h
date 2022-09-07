#ifndef findPeaksCPP_H
#define findPeaksCPP_H
using namespace Rcpp;
//' @title Ffind peaks using C++
//' @param vY Numeric vector or timeseries
//' @param m Defaults as 3, set this to define window over which to determine peaks
//' [[Rcpp::export]]
NumericVector findPeaksCPP( NumericVector vY, int m = 3);

#endif // findPeaksCPP_H
