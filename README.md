
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
      - Limited to 2500 time steps
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

This is a basic example which shows you how to solve a common problem:

``` r
library(HydroEnR)
## basic example code
```

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
