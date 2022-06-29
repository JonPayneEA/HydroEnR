QMED.numeric <- function(x, ...) {
  x <- median(x, na.rm = TRUE)
  class(x) <- append(class(x), 'QMED')
  return(x)
}

QMED.HydroAggsmax <- function(x, ...){
  x <- median(x$Hydro_year$Hydro_year_Max, na.rm = TRUE)
  class(x) <- append(class(x), 'QMED')
  return(x)
}

QMED.HydroAMAX <- function(x, ...){
  x <- median(x$AMAX, na.rm = TRUE)
  class(x) <- append(class(x), 'QMED')
  return(x)
}

QMED.FlowLoad <- function(x, ...){
  x <- GetAMAX(x)
  x <- median(x$AMAX, na.rm = TRUE)
  class(x) <- append(class(x), 'QMED')
  return(x)
}

QMED <- function(x,...) {
  UseMethod('QMED', x)
}

# Fixing the print of Lcv class data
print.QMED <- function(x, ...) {
  attr(x, "class") <- NULL
  print.default(x, ...)
}

# QMED(Buildwas)
# QMED(Buildwas_Analysis)

QMEDPlot <- function(x, ...) {
  AMAX <- GetAMAX(x)
  QMED_flow <- QMED(AMAX)
  QMED <- noquote(paste("Estimated QMED:", QMED_flow, "cumecs"))
  p <- ggplot(AMAX, aes(x = Year, y = AMAX)) +
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


