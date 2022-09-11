# HydroEnR <img src="logo.png" align="right" width="120" />

## Welcome to HydroEnR!

This package is designed for members of Evidence and Risk who are developing PDM
models, applications can be extended into hydrological analyses. It enables;

  - Easy loading of data downloaded from WISKI
      - Carry out quality checks on these data  
      - Preserve metadata
      - Interactive visualisations
  - Aggregation of data by different periods and through various functions
    using rapid C++ based processes
  - Rain gauge weighting
    - Weighted average method
  - Peak detection in hydrological data
    - Various functions available
  - Conversion of flow and precipitation data to a volumetric measure over a 
    designated time step
  - Derivation of Thiessen/Voronoi polygons for use in our realtime flood forecast
    models
  - Download up-to-date meta data on EA gauges published on API
  - Carry out single site analyses
  - Determine baseflow from 15 minute resolution data
  - Review model performance using a greater range of objective functions than
    what is currently available
  - ARMA parameter inspection for stability in a real time forecasting
    context
      - Loads a local version of the shiny application
    
### Future Updates

  - Basic hydraulic equations such as the Mannings’ equation
      - Import cross sectional data and carry out rudimentary analyses
  - Importing recent flow, stage and rain gauge data via the EAs API
      - Limited to 2500 time steps
  - Impute missing data into PE series
  - Trend detection in hydrological data
  - ESS in the near future
