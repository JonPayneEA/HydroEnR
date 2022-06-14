L3.numeric <- function(x, ...) {
  camp <- sort(x)
  n <- length(camp)
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  p2 <- p1 * (pp-1)/(nn-1)
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  b2 <- sum(p2*camp)/n
  L3 <- 6 * b2 - 6 * b1 + b0
  class(L3) <- append(class(L3), 'L3')
  return(L3)
}

L3.HydroAggsmax <- function(x, ...){
  camp <- sort(x$Hydro_year$Hydro_year_Max)
  n <- length(camp)
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  p2 <- p1 * (pp-1)/(nn-1)
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  b2 <- sum(p2*camp)/n
  L3 <- 6 * b2 - 6 * b1 + b0
  class(L3) <- append(class(L3), 'L3')
  return(L3)
}

L3.HydroAMAX <- function(x, ...){
  camp <- sort(x$AMAX)
  n <- length(camp)
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  p2 <- p1 * (pp-1)/(nn-1)
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  b2 <- sum(p2*camp)/n
  L3 <- 6 * b2 - 6 * b1 + b0
  class(L3) <- append(class(L3), 'L3')
  return(L3)
}

L3.FlowLoad <- function(x, ...){
  x <- GetAMAX(x)
  camp <- sort(x$AMAX)
  n <- length(camp)
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  p2 <- p1 * (pp-1)/(nn-1)
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  b2 <- sum(p2*camp)/n
  L3 <- 6 * b2 - 6 * b1 + b0
  class(L3) <- append(class(L3), 'L3')
  return(L3)
}

L3 <- function(x, ...) {
  UseMethod('L3', x)
}

# Fixing the print of Lcv class data
print.L3 <- function(x, ...) {
  attr(x, "class") <- NULL
  print.default(x, ...)
}

L3(Buildwas)
L3(Buildwas_Analysis)
L3(GetAMAX(Buildwas))
L3(Buildwas_Analysis$Hydro_year$Hydro_year_Max)
