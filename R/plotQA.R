# QA plots
library(dygraphs)

plotQA.rainAll <- function(x, ...) {
  dygraph(x, main = "Cumulative QA plot of Rain Gauges")  %>% 
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
    dyAxis("x", label = 'Date Time', drawGrid = FALSE) %>%
    dyAxis("y", label = 'Rainfall (mm)') %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>% 
    dyRangeSelector()
}

plotQA.rainAllDaily <- function(x, ...) {
  dygraph(x, main = "Cumulative QA plot of Rain Gauges")  %>% 
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
    dyAxis("x", label = 'Date Time', drawGrid = FALSE) %>%
    dyAxis("y", label = 'Rainfall (mm)') %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>% 
    dyRangeSelector()
}

plotQA.rainAllMonthly <- function(x, ...) {
  dygraph(x, main = "Cumulative QA plot of Rain Gauges")  %>% 
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
    dyAxis("x", label = 'Date Time', drawGrid = FALSE) %>%
    dyAxis("y", label = 'Rainfall (mm)') %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>% 
    dyRangeSelector()
}

plotQA.rainAllAnnual <- function(x, ...) {
  dygraph(x, main = "Cumulative QA plot of Rain Gauges")  %>% 
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
    dyAxis("x", label = 'Date Time', drawGrid = FALSE) %>%
    dyAxis("y", label = 'Rainfall (mm)') %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>% 
    dyRangeSelector()
}

plotQA <- function(x, ...){
  useMethod(plotQA, x, ...)
}

# Example
plotQA(b)  

zzz %>% 
  window.rainAllDaily(start = '2008-01-13') %>% 
  cumsumNA.rainAll() %>% 
  plotQA()


cumsumNA.rainAll(window.rainAllDaily(zzz, start = '2005-06-12'))

