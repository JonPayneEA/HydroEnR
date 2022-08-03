#' @title LCA moment
#'
#' @description Calculates the LCA moment from AMAX data. Can also convert time
#' series data imported via HydroEnR into LCA moments.
#'
#' @param x AMAX or flow time series
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' data <- getAMAX(rnrfa::get_ts(id = 2001, type = 'amax-flow'))
#' LCA(data)
LCA <- function(x, ...) {
  UseMethod('LCA', x)
}

#' @rdname LCA
#' @export
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

#' @rdname LCA
#' @export
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

#' @rdname LCA
#' @export
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

#' @rdname LCA
#' @export
LCA.flowLoad <- function(x, ...){
  x <- getAMAX(x)
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

#' @rdname LCA
#' @export
LCA.zoo <- function(x, ...){
  x <- getAMAX(x)
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

