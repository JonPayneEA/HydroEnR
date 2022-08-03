#' @title LKur moment
#'
#' @description Calculates the LKur moment from AMAX data. Can also convert time
#' series data imported via HydroEnR into LKur moments.
#'
#' @param x AMAX or flow time series
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' data <- getAMAX(rnrfa::get_ts(id = 2001, type = 'amax-flow'))
#' LKur(data)
LKur <- function(x, ...) {
  UseMethod('LKur', x)
}

#' @rdname LKur
#' @export
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

#' @rdname LKur
#' @export
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

#' @rdname LKur
#' @export
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

#' @rdname LKur
#' @export
LKur.flowLoad <- function(x, ...){
  x <- getAMAX(x)
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

#' @rdname LKur
#' @export
LKur.zoo <- function(x, ...){
  x <- getAMAX(x)
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

