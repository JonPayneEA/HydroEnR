# QA plots
library(dygraphs)

plotQA.rainAll <- function(x, ...) {
  dygraph(x, main = "Raw data plot of Rain Gauges")  %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
    dyAxis("x", label = 'Date Time', drawGrid = FALSE) %>%
    dyAxis("y", label = 'Rainfall (mm)') %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
    # dyBarChart() %>%
    dyRangeSelector()
}

plotQA.rainAllDaily <- function(x, ...) {
  dygraph(x, main = "Daily plot of Rain Gauges")  %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
    dyAxis("x", label = 'Date Time', drawGrid = FALSE) %>%
    dyAxis("y", label = 'Rainfall (mm)') %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
    # dyBarChart() %>%
    dyRangeSelector()
}

plotQA.rainAllMonthly <- function(x, ...) {
  dygraph(x, main = "Monthly plot of Rain Gauges")  %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
    dyAxis("x", label = 'Date Time', drawGrid = FALSE) %>%
    dyAxis("y", label = 'Rainfall (mm)') %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
    # dyBarChart() %>%
    dyRangeSelector()
}

plotQA.rainAllAnnual <- function(x, ...) {
  dygraph(x, main = "Annual plot of Rain Gauges")  %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
    dyAxis("x", label = 'Date Time', drawGrid = FALSE) %>%
    dyAxis("y", label = 'Rainfall (mm)') %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
    # dyBarChart() %>%
    dyRangeSelector()
}

plotQA.cumulRain <- function(x, ...) {
  dygraph(x, main = "Cumulative QA plot of Rain Gauges")  %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
    dyAxis("x", label = 'Date Time', drawGrid = FALSE) %>%
    dyAxis("y", label = 'Rainfall (mm)') %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
    dyRangeSelector()
}

plotQA <- function(x, ...){
  UseMethod('plotQA', x)
}

# Example
plotQA(b)

rain %>%
  window.rainAll(start = '2021-06-16 18:00', end = '2021-06-20 00:00') %>%
  # cumsumNA.rainAll() %>%
  plotQA()

plotQA(zzz)
plotQA(cumsumNA.rainAll(window.rainAllDaily(zzz, start = '2005-06-12')))

plotQA(window(zzz, start = '2005-06-12'))

