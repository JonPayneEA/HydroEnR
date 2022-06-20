LComponents.numeric <- function(x, ...) {
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
  cs <- data.frame(n, b0, b1, b2, b3)
  class(cs) <- append(class(cs), 'LCs')
  return(cs)
}

LComponents.HydroAggsmax <- function(x, ...){
  x <- x$Hydro_year$Hydro_year_Max
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
  cs <- data.frame(n, b0, b1, b2, b3)
  class(cs) <- append(class(cs), 'LCs')
  return(cs)
}

LComponents.HydroAMAX <- function(x, ...){
  x <- mean(x$AMAX, na.rm = TRUE)
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
  cs <- data.frame(n, b0, b1, b2, b3)
  class(cs) <- append(class(cs), 'LCs')
  return(cs)
}

LComponents.FlowLoad <- function(x, ...){
  x <- GetAMAX(x)
  ccamp <- sort(x$AMAX)
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
  cs <- data.frame(n, b0, b1, b2, b3)
  class(cs) <- append(class(cs), 'LCs')
  return(cs)
}

LComponents <- function(x, ...) {
  UseMethod('LComponents', x)
}

# Fixing the print of Lcv class data
print.LComponents <- function(x, ...) {
  attr(x, "class") <- NULL
  print.default(x, ...)
}

# LComponents(Buildwas)
# LComponents(Buildwas_Analysis)
# LComponents(GetAMAX(Buildwas))
# LComponents(Buildwas_Analysis$Hydro_year$Hydro_year_Max)
