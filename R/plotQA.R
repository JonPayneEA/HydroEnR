library(dygraphs)
#' @title plotQA
#'
#' @description Generic function for QA plots of HydroEnR objects.
#'
#' @description Simple QA plots for various objects. Plotting adapts for the data type and accumulation.
#'
#' @param x A data.table, list, or various other data type developed in the HydroEnR package
#' @param ... Include extra graphing parameters
#'
#' @return
#' @export
#'
#' @examples
#' plotQA(b)
#
#' rain %>%
#   window.rainAll(start = '2021-06-16 18:00', end = '2021-06-20 00:00') %>%
#   # cumsumNA.rainAll() %>%
#   plotQA()
#
#' plotQA(zzz)
#' plotQA(cumsumNA.rainAll(window.rainAllDaily(zzz, start = '2005-06-12')))
#
#' plotQA(window(zzz, start = '2005-06-12'))
plotQA <- function(x, ...){
  UseMethod('plotQA', x)
}

#' @rdname plotQA
#' @export
plotQA.rainAll <- function(x, ...) {
  dygraph(x, main = "Raw data plot of Rain Gauges")  %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
    dyAxis("x", label = 'Date Time', drawGrid = FALSE) %>%
    dyAxis("y", label = 'Rainfall (mm)') %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
    # dyBarChart() %>%
    dyRangeSelector()
}

#' @rdname plotQA
#' @export
plotQA.rainAllDaily <- function(x, ...) {
  dygraph(x, main = "Daily plot of Rain Gauges")  %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
    dyAxis("x", label = 'Date Time', drawGrid = FALSE) %>%
    dyAxis("y", label = 'Rainfall (mm)') %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
    # dyBarChart() %>%
    dyRangeSelector()
}

#' @rdname plotQA
#' @export
plotQA.rainAllMonthly <- function(x, ...) {
  dygraph(x, main = "Monthly plot of Rain Gauges")  %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
    dyAxis("x", label = 'Date Time', drawGrid = FALSE) %>%
    dyAxis("y", label = 'Rainfall (mm)') %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
    # dyBarChart() %>%
    dyRangeSelector()
}

#' @rdname plotQA
#' @export
plotQA.rainAllAnnual <- function(x, ...) {
  dygraph(x, main = "Annual plot of Rain Gauges")  %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
    dyAxis("x", label = 'Date Time', drawGrid = FALSE) %>%
    dyAxis("y", label = 'Rainfall (mm)') %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
    # dyBarChart() %>%
    dyRangeSelector()
}

#' @rdname plotQA
#' @export
plotQA.cumulRain <- function(x, ...) {
  dygraph(x, main = "Cumulative QA plot of Rain Gauges")  %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
    dyAxis("x", label = 'Date Time', drawGrid = FALSE) %>%
    dyAxis("y", label = 'Rainfall (mm)') %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
    dyRangeSelector()
}

# # Example


