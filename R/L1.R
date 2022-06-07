L1.default <- function(x, ...) {
  x <- mean(x, na.rm = TRUE)
  class(x) <- append(class(x), 'L1')
  return(x)
}

L1.HydroAggsmax <- function(x, ...){
  x <- mean(x$Hydro_year$HydroYear_Max, na.rm = TRUE)
  class(x) <- append(class(x), 'L1')
  return(x)
}

L1.HydroAMAX <- function(x, ...){
  x <- mean(x$AMAX, na.rm = TRUE)
  class(x) <- append(class(x), 'L1')
  return(x)
}

L1.FlowLoad <- function(x, ...){
  x <- GetAMAX(x)
  x <- mean(x$AMAX, na.rm = TRUE)
  class(x) <- append(class(x), 'L1')
  return(x)
}

L1 <- function(x, ...) {
  UseMethod('L1', x)
}

# Fixing the print of Lcv class data
print.L1 <- function(x, ...) {
  attr(x, "class") <- NULL
  print.default(x, ...)
}

L1(Buildwas)
L1(Buildwas_Analysis)
L1(GetAMAX(Buildwas))
L1(Buildwas_Analysis$Hydro_year$HydroYear_Max)
