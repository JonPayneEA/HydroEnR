LCA.numeric <- function(x, ...) {
  camp <- sort(x)
  n <- length(camp)
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  p2 <- p1 * (pp-1)/(nn-1)
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  b2 <- sum(p2*camp)/n
  lca <- 2*(3*b2-b0)/(2*b1-b0)-3
  class(lca) <- append(class(lca), 'LCA')
  return(lca)
}

LCA.HydroAggsmax <- function(x, ...){
  camp <- sort(x$Hydro_year$Hydro_year_Max)
  n <- length(camp)
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  p2 <- p1 * (pp-1)/(nn-1)
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  b2 <- sum(p2*camp)/n
  lca <- 2*(3*b2-b0)/(2*b1-b0)-3
  class(lca) <- append(class(lca), 'LCA')
  return(lca)
}

LCA.HydroAMAX <- function(x, ...){
  camp <- sort(x$AMAX)
  n <- length(camp)
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  p2 <- p1 * (pp-1)/(nn-1)
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  b2 <- sum(p2*camp)/n
  lca <- 2*(3*b2-b0)/(2*b1-b0)-3
  class(lca) <- append(class(lca), 'LCA')
  return(lca)
}

LCA.FlowLoad <- function(x, ...){
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
  lca <- 2*(3*b2-b0)/(2*b1-b0)-3
  class(lca) <- append(class(lca), 'LCA')
  return(lca)
}

LCA <- function(x, ...) {
  UseMethod('LCA', x)
}

# Fixing the print of Lcv class data
print.LCA <- function(x, ...) {
  attr(x, "class") <- NULL
  print.default(x, ...)
}

LCA(Buildwas)
LCA(Buildwas_Analysis)
LCA(GetAMAX(Buildwas))
LCA(Buildwas_Analysis$Hydro_year$Hydro_year_Max)
