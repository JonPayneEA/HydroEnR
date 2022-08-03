#' @title L4 moment
#'
#' @description Calculates the L4 moment from AMAX data. Can also convert time
#' series data imported via HydroEnR into L4 moments.
#'
#' @param x AMAX or flow time series
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' data <- getAMAX(rnrfa::get_ts(id = 2001, type = 'amax-flow'))
#' L4(data)
L4 <- function(x, ...) {
  UseMethod('L4', x)
}

#' @rdname L4
#' @export
L4.numeric <- function(x, ...) {
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
  L4 <- 20 * b3 - 30 * b2 + 12 * b1 - b0
  class(L4) <- append(class(L4), 'L4')
  return(L4)
}

#' @rdname L4
#' @export
L4.HydroAggsmax <- function(x, ...){
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
  L4 <- 20 * b3 - 30 * b2 + 12 * b1 - b0
  class(L4) <- append(class(L4), 'L4')
  return(L4)
}

#' @rdname L4
#' @export
L4.HydroAMAX <- function(x, ...){
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
  L4 <- 20 * b3 - 30 * b2 + 12 * b1 - b0
  class(L4) <- append(class(L4), 'L4')
  return(L4)
}

#' @rdname L4
#' @export
L4.fowLoad <- function(x, ...){
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
  L4 <- 20 * b3 - 30 * b2 + 12 * b1 - b0
  class(L4) <- append(class(L4), 'L4')
  return(L4)
}

#' @rdname L4
#' @export
L4.zoo <- function(x, ...){
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
  L4 <- 20 * b3 - 30 * b2 + 12 * b1 - b0
  class(L4) <- append(class(L4), 'L4')
  return(L4)
}
#' @rdname L4
#' @export
print.L4 <- function(x, ...) {
  # Fixing the print of L4 class data
  attr(x, "class") <- NULL
  print.default(x, ...)
}
