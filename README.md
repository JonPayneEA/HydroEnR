
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HydroEnR <img src="logo.png" align="right" width="120" />

<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![](https://img.shields.io/github/last-commit/JonPayneEA/HydroEnR.svg)](https://github.com/JonPayneEA/HydroEnR/commits/main)
[![License: GNU General Public
License](https://img.shields.io/badge/license-GNU%20General%20Public%20License-blue.svg)](https://cran.r-project.org/web/licenses/GNU%20General%20Public%20License)
[![](https://img.shields.io/github/languages/code-size/JonPayneEA/HydroEnR.svg)](https://github.com/JonPayneEA/HydroEnR)
<!-- badges: end -->

# Welcome to HydroEnR\!

This package is designed for members of Evidence and Risk who are
developing PDM models, applications can be extended into hydrological
analyses. It enables;

  - Easy loading of data downloaded from WISKI
      - Carry out quality checks on these data  
      - Preserve metadata
      - Interactive visualisations
  - Aggregation of data by different periods and through various
    functions using rapid C++ based processes
  - Rain gauge weighting
      - Weighted average method
  - Peak detection in hydrological data
      - Various functions available
  - Conversion of flow and precipitation data to a volumetric measure
    over a designated time step
  - Derivation of Thiessen/Voronoi polygons for use in our realtime
    flood forecast models
  - Download up-to-date meta data on EA gauges published on API
  - Carry out single site analyses
  - Determine baseflow from 15 minute resolution data
  - Review model performance using a greater range of objective
    functions than what is currently available
  - ARMA parameter inspection for stability in a real time forecasting
    context
      - Loads a local version of the shiny application

# Future Updates

  - Basic hydraulic equations such as the Mannings’ equation
      - Import cross sectional data and carry out rudimentary analyses
  - Importing recent flow, stage and rain gauge data via the EAs API
  - Impute missing data into PE series
  - Trend detection in hydrological data
  - ESS in the near future

## Installation

You can install the development version of HydroEnR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JonPayne88/HydroEnR")
```

## Example

This is a basic example which shows you how to fit a rating curve to
observed data.

``` r
library(HydroEnR)
## basic example code
```

``` r
# Plot raw rating data

plot(Discharge ~ Stage)
```

<img src="man/figures/README-rawplot-1.png" width="100%" />

``` r
# Optimise rating with a control points at 1.6m and 2.3m
rateOptim <- rateOptimise(Discharge, Stage, control = c(1.6, 2.3))
rateOptim
#> $Meta
#>      Limb lowerRange upperRange minStage maxStage minDischarge maxDischarge
#>    <fctr>      <num>      <num>    <num>    <num>        <num>        <num>
#> 1:      1       1.08       1.60    1.085    1.600        0.547         10.2
#> 2:      2       1.60       2.30    1.601    2.296        8.390         37.3
#> 3:      3       2.30       2.69    2.307    2.689       35.300         53.2
#> 
#> $Data
#>        Discharge Stage   Limb      Range lowerRange upperRange
#>            <num> <num> <fctr>     <fctr>      <num>      <num>
#>     1:     0.547 1.103      1 [1.08,1.6]       1.08       1.60
#>     2:     0.579 1.085      1 [1.08,1.6]       1.08       1.60
#>     3:     0.589 1.088      1 [1.08,1.6]       1.08       1.60
#>     4:     0.589 1.089      1 [1.08,1.6]       1.08       1.60
#>     5:     0.593 1.089      1 [1.08,1.6]       1.08       1.60
#>    ---                                                        
#> 18826:    49.300 2.629      3 (2.3,2.69]       2.30       2.69
#> 18827:    50.800 2.655      3 (2.3,2.69]       2.30       2.69
#> 18828:    51.400 2.685      3 (2.3,2.69]       2.30       2.69
#> 18829:    52.000 2.685      3 (2.3,2.69]       2.30       2.69
#> 18830:    53.200 2.689      3 (2.3,2.69]       2.30       2.69
#> 
#> $`NLS Limb 1`
#> Nonlinear regression model
#>   model: Discharge ~ C * (Stage + a)^n
#>    data: dtl
#>       C       a       n 
#> 20.2738 -0.8566  2.5019 
#>  residual sum-of-squares: 873.9
#> 
#> Number of iterations to convergence: 27 
#> Achieved convergence tolerance: 1.49e-08
#> 
#> $`NLS Limb 2`
#> Nonlinear regression model
#>   model: Discharge ~ C * (Stage + a)^n
#>    data: dtl
#>      C      a      n 
#> 34.597 -1.224  1.332 
#>  residual sum-of-squares: 1705
#> 
#> Number of iterations to convergence: 38 
#> Achieved convergence tolerance: 1.49e-08
#> 
#> $`NLS Limb 3`
#> Nonlinear regression model
#>   model: Discharge ~ C * (Stage + a)^n
#>    data: dtl
#>       C       a       n 
#>  8.6308 -0.2623  2.0195 
#>  residual sum-of-squares: 85.17
#> 
#> Number of iterations till stop: 93 
#> Achieved convergence tolerance: 1.49e-08
#> Reason stopped: Number of calls to `fcn' has reached or exceeded `maxfev' == 400.
```

``` r
# Plot the fittings
ratingPlot(rateOptim, colours = c(2, 3, 4))
```

<img src="man/figures/README-fittedplot-1.png" width="100%" />

<!-- What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so: -->

<!-- ```{r cars} -->

<!-- summary(cars) -->

<!-- ``` -->

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>. -->

<!-- You can also embed plots, for example: -->

<!-- ```{r pressure, echo = FALSE} -->

<!-- plot(pressure) -->

<!-- ``` -->

<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->
