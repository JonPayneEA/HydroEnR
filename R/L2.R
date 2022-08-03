#' @title L2 moment
#'
#' @description Calculates the L2 moment from AMAX data. Can also convert time
#' series data imported via HydroEnR into L2 moments.
#'
#' @param x AMAX or flow time series
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' data <- getAMAX(rnrfa::get_ts(id = 2001, type = 'amax-flow'))
#' L2(data)
L2 <- function(x, ...) {
  UseMethod('L2', x)
}

#' @rdname L2
#' @export
L2.numeric <- function(x, ...) {
  camp <- sort(x)
  n <- length(camp)
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  l2 <- 2*b1-b0
  class(l2) <- append(class(l2), 'L2')
  return(l2)
}

#' @rdname L2
#' @export
L2.HydroAggsmax <- function(x, ...){
  camp <- sort(x$Hydro_year$Hydro_year_Max)
  n <- length(camp)
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  l2 <- 2*b1-b0
  class(l2) <- append(class(l2), 'L2')
  return(l2)
}

#' @rdname L2
#' @export
L2.HydroAMAX <- function(x, ...){
  camp <- sort(x$AMAX)
  n <- length(camp)
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  l2 <- 2*b1-b0
  class(l2) <- append(class(l2), 'L2')
  return(l2)
}

#' @rdname L2
#' @export
L2.flowLoad <- function(x, ...){
  x <- getAMAX(x)
  camp <- sort(x$AMAX)
  n <- length(camp)
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  l2 <- 2*b1-b0
  class(l2) <- append(class(l2), 'L2')
  return(l2)
}

#' @rdname L2
#' @export
# Fixing the print of L2 class data
print.L2 <- function(x, ...) {
  attr(x, "class") <- NULL
  print.default(x, ...)
}

