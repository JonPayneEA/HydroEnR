#' @name Objectives
#' @rdname Objectives
#'
#' @title Model diagnostics
#'
#' @description Calculate model diagnostics from an observed series, x, and
#' modelled series, y.
#'
#' @usage R2 (x, y, na.rm=FALSE)
#' RMSE (x, y, na.rm=FALSE)
#' MAE (x, y, na.rm=FALSE)
#' RMSEP (x, y, na.rm=FALSE)
#' MAEP (x, y, na.rm=FALSE)
#'
#' @param x Observed data series
#' @param y Estimated/modelled values
#' @param na.rm NA remove defaulted as FALSE
#'
#' @return
#' @export
#'
#' @examples
#' R2(x, y)
#' RMSE(x, y)
#' MAE(x, y)
#' RMSEP(x, y)
#' MAEP(x, y)
R2 <- function(x, y, na.rm=FALSE) {

  # INPUT
  # x = observed values
  # y = estimated values

  if (na.rm==TRUE) {
    y <- y[!is.na(x)]
    x <- x[!is.na(x)]
    x <- x[!is.na(y)]
    y <- y[!is.na(y)]
  }
  n <- length(x)
  if (!length(y)==n) stop("R2: x and y must have the same length")
  SST <- sum((x-mean(x))^2)
  SSRes <- sum((x-y)^2)
  R2 <- 1-SSRes/SST

  return(R2)
}

#' @rdname RMSE
#' @export
RMSE <- function(x, y, na.rm=FALSE) {

  # INPUT
  # x = observed values
  # y = estimated values

  if (na.rm==TRUE) {
    y <- y[!is.na(x)]
    x <- x[!is.na(x)]
    x <- x[!is.na(y)]
    y <- y[!is.na(y)]
  }
  n <- length(x)
  if (!length(y)==n) stop("RMSE: x and y must have the same length")
  res <- x-y
  RMSE <- sqrt(sum((res)^2)/n)

  return(RMSE)
}

#' @rdname MAE
#' @export
MAE <- function(x, y, na.rm=FALSE) {

  # INPUT
  # INPUT
  # x = observed values
  # y = estimated values

  if (na.rm==TRUE) {
    y <- y[!is.na(x)]
    x <- x[!is.na(x)]
    x <- x[!is.na(y)]
    y <- y[!is.na(y)]
  }
  n <- length(x)
  if (!length(y)==n) stop("MAE: x and y must have the same length")
  res <- x-y
  MAE <- sum(abs(res))/n

  return(MAE)
}

#' @rdname RMSEP
#' @export
RMSEP <- function(x, y, na.rm=FALSE) {

  # INPUT
  # x = observed values
  # y = estimated values

  if (na.rm==TRUE) {
    y <- y[!is.na(x)]
    x <- x[!is.na(x)]
    x <- x[!is.na(y)]
    y <- y[!is.na(y)]
  }
  n <- length(x)
  if (!length(y)==n) stop("RMSE: x and y must have the same length")
  res <- (x-y)/x
  RMSEP <- sqrt(sum((res)^2)/n)

  return(RMSEP)
}

#' @rdname MAEP
#' @export
MAEP <- function(x, y, na.rm=FALSE) {

  # INPUT
  # INPUT
  # x = observed values
  # y = estimated values

  if (na.rm==TRUE) {
    y <- y[!is.na(x)]
    x <- x[!is.na(x)]
    x <- x[!is.na(y)]
    y <- y[!is.na(y)]
  }
  n <- length(x)
  if (!length(y)==n) stop("MAE: x and y must have the same length")
  res <- (x-y)/x
  MAEP <- sum(abs(res))/n

  return(MAEP)
}
