QMED.default <- function(x, ...) {
  x <- median(x, na.rm = TRUE)
  class(x) <- append(class(x), 'QMED')
  return(x)
}

QMED.HydroAggsmax <- function(x, ...){
  x <- median(x$Hydro_year$HydroYear_Max, na.rm = TRUE)
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


QMED(Buildwas)
QMED(Buildwas_Analysis)


QMEDPlot.HydroAggsmax <- function(x, ...) {
  QMED_flow<- median(x$Hydro_year$HydroYear_Max, na.rm = TRUE)
  QMED <- noquote(paste("Estimated QMED:", QMED_flow, "cumecs"))
  p <- ggplot(x$Hydro_year, aes(x = HydrologicalYear, y = HydroYear_Max)) +
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

QMEDPlot<- function(x,...) {
  UseMethod('QMEDPlot', x)
}

a <- QMEDPlot(Buildwas_Analysis)
a


zzz <- as.data.frame(Buildwas_Analysis[['Hydro_year']])
zz <-data.frame(x = c(1:100), y = rnorm(100,1))



ggplot(zzz, aes(x = HydrologicalYear, y = HydroYear_Max)) +
  geom_point()

  xlab('Hydrological Year') 
  ylab(expression(Flow ~ m^3 ~ s^-1)) +
  ggtitle('AMAX flow by hydrological year') 
