#' @title L3 moment
#'
#' @description Calculates the L3 moment from AMAX data. Can also convert time
#' series data imported via HydroEnR into L3 moments.
#'
#' @param x AMAX or flow time series
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' data <- getAMAX(rnrfa::get_ts(id = 2001, type = 'amax-flow'))
#' L3(data)
L3 <- function(x, ...) {
  UseMethod('L3', x)
}

#' @rdname L3
#' @export
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

#' @rdname L3
#' @export
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

#' @rdname L3
#' @export
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

#' @rdname L3
#' @export
L3.flowLoad <- function(x, ...){
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
  L3 <- 6 * b2 - 6 * b1 + b0
  class(L3) <- append(class(L3), 'L3')
  return(L3)
}

#' @rdname L3
#' @export
L3.zoo <- function(x, ...){
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
  L3 <- 6 * b2 - 6 * b1 + b0
  class(L3) <- append(class(L3), 'L3')
  return(L3)
}
#' @rdname L3
#' @export
print.L3 <- function(x, ...) {
  # Fixing the print of L3 class data
  attr(x, "class") <- NULL
  print.default(x, ...)
}

