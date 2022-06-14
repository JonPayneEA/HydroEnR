L2.default <- function(x, ...) {
  camp <- sort(x)
  n <- length(camp)
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  l2 <- 2*b1-b0
  class(l2) <- 'L2'
  return(l2)
}

L2.HydroAggsmax <- function(x, ...){
  camp <- sort(x$Hydro_year$Hydro_year_Max)
  n <- length(camp)
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  l2 <- 2*b1-b0
  class(l2) <- 'L2'
  return(l2)
}

L2.HydroAMAX <- function(x, ...){
  camp <- sort(x$AMAX)
  n <- length(camp)
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  l2 <- 2*b1-b0
  class(l2) <- 'L2'
  return(l2)
}

L2.FlowLoad <- function(x, ...){
  x <- GetAMAX(x)
  camp <- sort(x$AMAX)
  n <- length(camp)
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  l2 <- 2*b1-b0
  class(l2) <- 'L2'
  return(l2)
}

L2 <- function(x, ...) {
  UseMethod('L2', x)
}

# Fixing the print of Lcv class data
print.L2 <- function(x, ...) {
  attr(x, "class") <- NULL
  print.default(x, ...)
}

L2(Buildwas)
L2(Buildwas_Analysis)
L2(GetAMAX(Buildwas))
L2(Buildwas_Analysis$Hydro_year$Hydro_year_Max)
