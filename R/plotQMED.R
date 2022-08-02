library(ggplot2)
#' @title plotQMED
#'
#' @description Calculates and generates a plot of the QMED estimate relative
#' to the AMAX series.
#'
#' @param x A series of flow data loaded into the environment via HydroEnR
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' data <- rnrfa::get_ts(id = 2001, type = 'amax-flow') %>% getAMAX()
#' plotQMED(data)
plotQMED <- function(x, ...) {
  AMAX <- getAMAX(x)
  QMED_flow <- QMED(AMAX)
  QMED <- noquote(paste("Estimated QMED:", QMED_flow, "cumecs"))
  p <- ggplot2::ggplot(AMAX, aes(x = Year, y = AMAX)) +
    geom_line(size = 1.2) +
    xlab('Hydrological Year') +
    ylab(expression(Flow ~ m^3 ~ s^-1)) +
    ggtitle('AMAX flow by hydrological year') +
    geom_hline(yintercept = QMED_flow, colour = '#00A33B', size = 2) +
    theme_light()
  beepr::beep(sound = sample(1:11, 1), expr = "WOW")
  print(QMED)
  p
}
