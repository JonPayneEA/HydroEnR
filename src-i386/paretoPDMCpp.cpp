#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
List PDM(double fc, double Cmin, double Cmax, double b, NumericVector P, NumericVector PE, double s, double be,
         double kg, double St, double bg, double k1, double k2, double kb, double area){
  double k = 1/kg;
  double Smax = (b * Cmin + Cmax) / (b + 1);
  int n = P.size();
  NumericVector total(n);
  NumericVector storage(n);
  NumericVector store_fullness(n);
  NumericVector AE_PE(n);
  NumericVector evaporation(n);
  NumericVector C_star(n);
  NumericVector prop_runoff(n);
  NumericVector runoff(n);
  NumericVector drainage(n);
  storage[0] = s;
  store_fullness[0] = storage[0]/Smax;
  AE_PE[0] = 1 - pow(1 - store_fullness[0], be);
  C_star[0] = Cmin + (Cmax - Cmin) * (1 - pow(((Smax - storage[0]) / (Smax - Cmin)),(1/(b + 1))));
  if (1 - pow(((Cmax - C_star[0])/(Cmax-Cmin)),b) > 0){
    prop_runoff[0] = 1 - pow(((Cmax - C_star[0])/(Cmax-Cmin)),b);
  } else {
    prop_runoff[0] = 0;
  }
  if (storage[0] <= St) {
    drainage[0] = 0;
  } else {
    drainage[0] = k * pow((storage[0] - St), bg);
  }
  for(int i = 1; i < n; ++i) {
    if (storage[i-1] <= St) {
      drainage[i-1] = 0;
    } else {
      drainage[i-1] = k * pow((storage[i-1] - St), bg);
    }
    AE_PE[i-1] = 1 - pow(1 - store_fullness[i-1], be);
    evaporation[i-1] = PE[i-1] * AE_PE[i-1];
    if (storage[i-1] + (P[i-1]-(P[i-1]*prop_runoff[i-1])) - drainage[i-1] - evaporation[i-1] < 0){
      storage[i] = 0;
    } else {
      storage[i] = storage[i-1] + (P[i-1]-(P[i-1]*prop_runoff[i-1])) - drainage[i-1] - evaporation[i-1];
    }
    if(storage[i] >= Smax){
      storage[i] = Smax;
    }
    store_fullness[i] = storage[i]/Smax;
    C_star[i] = Cmin + (Cmax - Cmin) * (1 - pow(((Smax - storage[i-1]) / (Smax - Cmin)),(1/(b + 1))));
    if (1 - pow(((Cmax - C_star[i])/(Cmax-Cmin)),b) > 0){
      prop_runoff[i] = 1 - pow(((Cmax - C_star[i])/(Cmax-Cmin)),b);
    } else {
      prop_runoff[i] = 0;
    }
    runoff[i] =  P[i] * prop_runoff[i];
  }
  int dt = 0.25;
  int s1_alpha = -(exp((-dt/k1)));
  int s1_beta = 1 + s1_alpha;
  int s2_alpha = -(exp((-dt/k2)));
  int s2_beta = 1 + s2_alpha;
  int b0 = s1_beta * s2_beta;
  int a1 = s1_alpha + s2_alpha;
  int a2 = s1_alpha * s2_alpha;
  NumericVector rain(n + 2);
  NumericVector flow(n + 2);
  for(int i = 2; i < n; ++i) {
    flow[i] = -a1*flow[i-1] - a2 * flow [i-2] + b0 * rain[i-1]; // Loop to calculate rapid run off
  }
  flow = flow * area;
  // Cubic store
  // cubic_k = 1/pow(kb, 3) # k value for cubic store
  // NumericVector drainage_to_cubic(drainage_to_cubic) // Drainage data
  // int initial_store = 10 # Arbitrary number, initial storage value
  // baseflow = cubic_k*pow(initial_store[0],3) // Initial baseflow
  // baseflow =c(baseflow, rep(0, length(drainage_to_cubic))) // Dummmy baseflow series, rows 2 onwards will be over written
  // cubic_store = c(initial_store, rep(0, output)) // Dummy baseflow series, rows 2 onwards will be overwritten
  // for(i in seq_along(drainage_to_cubic)){
  //   cubic_store[i+1] = cubic_store[i]-(1/(3*cubic_k*(cubic_store[i]^2)))*(exp(-3*cubic_k*(cubic_store[i]^2)*dt)-1)*(drainage_to_cubic[i+1] - cubic_k*(cubic_store[i] ^3))
  //   cubic_store[i+1] = ifelse(cubic_store[i+1] < 0, 0, cubic_store[i+1]) # Minimises cubic store to zero, prevents negative baseflow
  //   baseflow[i+1] = cubic_k*(cubic_store[i+1]^3) + Qconst # Addition of Qconst to baseflow
  //     }
  List ret;
  ret["total"] = total;
  ret["Smax"] = Smax;
  ret["storage"] = storage;
  ret["AE_PE"] = AE_PE;
  ret["evaporation"] = evaporation;
  ret["C_star"] = C_star;
  ret["prop_runoff"] = prop_runoff;
  ret["runoff"] = runoff;
  ret["drainage"] = drainage;
  ret["flow"] = flow;

  return ret;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
# library(Rcpp)
# library(microbenchmark)
# library(tidyverse)
#
# alf_flow <- readr::read_csv("O:/National Modelling and Forecasting/21_Strategic Delivery/Flood Forecast Modelling/03 Training/PDM_in_R/Alfoldean/Flow.csv", col_names = FALSE)  # Flow
# alf_PE <- readr::read_csv("O:/National Modelling and Forecasting/21_Strategic Delivery/Flood Forecast Modelling/03 Training/PDM_in_R/Alfoldean/PE_r.csv", col_names = FALSE) # Amended PE
# alf_rain <- readr::read_csv("O:/National Modelling and Forecasting/21_Strategic Delivery/Flood Forecast Modelling/03 Training/PDM_in_R/Alfoldean/Rain.csv", col_names = FALSE) # Rain (single source)
#
# flow <- alf_flow$X7
# rain <- alf_rain$X7
#
# alf_PE <- alf_PE %>%
#   dplyr::slice(rep(1:n(), each = 96)) # Convert the daily PE data to 15 minute
#
# PE <- alf_PE$X7/96

# system.time({
#   a<-PDM(fc = 1, Cmin =  0, Cmax =  200, b = 1, P = rain, PE = PE, s = 20, be = 2, kg = 70000, St = 2, bg = 4.6,
#          k1 =8, k2 = 10, area = 154)
# })

# microbenchmark::microbenchmark(
#   a<-PDM(fc = 1, Cmin =  0, Cmax =  200, b = 1, P = rain, PE = PE, s = 20, be = 2, kg = 70000, St = 2, bg = 4.6,
#          k1 =8, k2 = 10, kb = 60, area = 154)
# )

# microbenchmark::microbenchmark(a<-PDM(fc = 1, Cmin =  0, Cmax =  200, b = 1, P = rain, PE = PE, s = 20, be = 2, kg = 70000, St = 2, bg = 4.6),
#                                Alfodean_RMSE <- PDM_basic_pareto(fc = 1.0, # Rainfall factor
#                                                                  Cmax = 65, # Maximum storage capacity
#                                                                  Cmin = 14, # Minimum storage capacity
#                                                                  b = 0.2098571, # Exponent of Pareto distribution controlling spatial variability of store capacity
#                                                                  b_e = 2.8324, # Exponent in actual evaporation function
#                                                                  k1 = 3.935875, # 1st time constant of cascade of two linear reservoirs
#                                                                  k2 = 14.69995, # 2nd time constant of cascade of two linear reservoirs
#                                                                  k_b = 71 , # Base flow time constant
#                                                                  k_g = 389843 ,  # Groundwater recharge component
#                                                                  St = 2, # Soil tension storage capacity
#                                                                  b_g = 4.6, # Exponent of the recharge function
#                                                                  Qconst = 0, # Constant baseflow addition
#                                                                  td = 0, # Pure time delay
#                                                                  sini = "Smax", # Initial soil store, defaults to Smax
#                                                                  Area = 154, # Catchment area
#                                                                  P = alf_rain$X7, # Precipitation
#                                                                  PE = adj_PE, # Potential evaporation series
#                                                                  Qobs = alf_flow$X7, # Observed flow
#                                                                  perf_offset = 2500, # Burn in period for model assessment
#                                                                  output = 100, # Number of time steps to provide in forecast
#                                                                  plot_title = "The River Arun at Alfodean"))

*/
