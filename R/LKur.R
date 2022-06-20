LKur.numeric <- function(x, ...) {
  camp <- sort(x)
  n <- length(camp)
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  p2 <- p1 * (pp-1)/(nn-1)
  p3 <- p2 * (pp-2)/(nn-2)
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  b2 <- sum(p2*camp)/n
  b3 <- sum(p3*camp)/n
  lkur <- 5*(2*(2*b3-3*b2)+b0)/(2*b1-b0)+6
  class(lkur) <- append(class(lkur), 'LKur')
  return(lkur)
}

LKur.HydroAggsmax <- function(x, ...){
  camp <- sort(x$Hydro_year$Hydro_year_Max)
  n <- length(camp)
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  p2 <- p1 * (pp-1)/(nn-1)
  p3 <- p2 * (pp-2)/(nn-2)
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  b2 <- sum(p2*camp)/n
  b3 <- sum(p3*camp)/n
  lkur <- 5*(2*(2*b3-3*b2)+b0)/(2*b1-b0)+6
  class(lkur) <- append(class(lkur), 'LKur')
  return(lkur)
}

LKur.HydroAMAX <- function(x, ...){
  camp <- sort(x$AMAX)
  n <- length(camp)
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  p2 <- p1 * (pp-1)/(nn-1)
  p3 <- p2 * (pp-2)/(nn-2)
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  b2 <- sum(p2*camp)/n
  b3 <- sum(p3*camp)/n
  lkur <- 5*(2*(2*b3-3*b2)+b0)/(2*b1-b0)+6
  class(lkur) <- append(class(lkur), 'LKur')
  return(lkur)
}

LKur.FlowLoad <- function(x, ...){
  x <- GetAMAX(x)
  camp <- sort(x$AMAX)
  n <- length(camp)
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  p2 <- p1 * (pp-1)/(nn-1)
  p3 <- p2 * (pp-2)/(nn-2)
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  b2 <- sum(p2*camp)/n
  b3 <- sum(p3*camp)/n
  lkur <- 5*(2*(2*b3-3*b2)+b0)/(2*b1-b0)+6
  class(lkur) <- append(class(lkur), 'LKur')
  return(lkur)
}

LKur <- function(x, ...) {
  UseMethod('LKur', x)
}

# Fixing the print of Lcv class data
print.LKur <- function(x, ...) {
  attr(x, "class") <- NULL
  print.default(x, ...)
}

# LKur(Buildwas)
# LKur(Buildwas_Analysis)
# LKur(GetAMAX(Buildwas))
# LKur(Buildwas_Analysis$Hydro_year$Hydro_year_Max)
