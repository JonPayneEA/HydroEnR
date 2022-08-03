#' @title L-Skew moment
#'
#' @description Calculates the L-Skew moment from AMAX data. Can also convert time
#' series data imported via HydroEnR into L-Skew moments.
#'
#' @param x AMAX or flow time series
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' data <- getAMAX(rnrfa::get_ts(id = 2001, type = 'amax-flow'))
#' LSKEW(data)
LSKEW <- function(x,...) {
  UseMethod('LSKEW', x)
}

#' @rdname LSKEW
#' @export
LSKEW.numeric <- function(x){
  Sort_x <- sort(x)
  ln <- length(x)
  Rank <- seq(1, ln)
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank - 1)/(ln - 1) * Sort_x, na.rm = TRUE)
  b2 <- mean(((Rank - 1) * (Rank - 2))/((ln - 1) * (ln - 2)) * Sort_x, na.rm = TRUE)
  b3 <- mean(((Rank - 1) * (Rank - 2) * (Rank - 3))/((ln - 1) * (ln - 2) * (ln - 3)) * Sort_x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2 * b1 - b0
  L3 <- 6 * b2 - 6 * b1 + b0
  LSKEW <- L3/L2
  class(LSKEW) <- append(class(LSKEW),'LSkew')
  return(LSKEW)
}

#' @rdname LSKEW
#' @export
LSKEW.HydroAggsmax <- function(x){
  x <- x$Hydro_year$Hydro_year_Max
  Sort_x <- sort(x)
  ln <- length(x)
  Rank <- seq(1, ln)
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank - 1)/(ln - 1) * Sort_x, na.rm = TRUE)
  b2 <- mean(((Rank - 1) * (Rank - 2))/((ln - 1) * (ln - 2)) * Sort_x, na.rm = TRUE)
  b3 <- mean(((Rank - 1) * (Rank - 2) * (Rank - 3))/((ln - 1) * (ln - 2) * (ln - 3)) * Sort_x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2 * b1 - b0
  L3 <- 6 * b2 - 6 * b1 + b0
  LSKEW <- L3/L2
  class(LSKEW) <- append(class(LSKEW),'LSkew')
  return(LSKEW)
}

#' @rdname LSKEW
#' @export
LSKEW.HydroAMAX <- function(x){
  x <- x$AMAX
  Sort_x <- sort(x)
  ln <- length(x)
  Rank <- seq(1, ln)
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank - 1)/(ln - 1) * Sort_x, na.rm = TRUE)
  b2 <- mean(((Rank - 1) * (Rank - 2))/((ln - 1) * (ln - 2)) * Sort_x, na.rm = TRUE)
  b3 <- mean(((Rank - 1) * (Rank - 2) * (Rank - 3))/((ln - 1) * (ln - 2) * (ln - 3)) * Sort_x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2 * b1 - b0
  L3 <- 6 * b2 - 6 * b1 + b0
  LSKEW <- L3/L2
  class(LSKEW) <- append(class(LSKEW),'LSkew')
  return(LSKEW)
}

#' @rdname LSKEW
#' @export
LSKEW.flowLoad <- function(x){
  x <- getAMAX(x)
  x <- x$AMAX
  Sort_x <- sort(x)
  ln <- length(x)
  Rank <- seq(1, ln)
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank - 1)/(ln - 1) * Sort_x, na.rm = TRUE)
  b2 <- mean(((Rank - 1) * (Rank - 2))/((ln - 1) * (ln - 2)) * Sort_x, na.rm = TRUE)
  b3 <- mean(((Rank - 1) * (Rank - 2) * (Rank - 3))/((ln - 1) * (ln - 2) * (ln - 3)) * Sort_x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2 * b1 - b0
  L3 <- 6 * b2 - 6 * b1 + b0
  LSKEW <- L3/L2
  class(LSKEW) <- append(class(LSKEW),'LSkew')
  return(LSKEW)
}

#' @rdname LSKEW
#' @export
LSKEW.zoo <- function(x){
  x <- getAMAX(x)
  x <- x$AMAX
  Sort_x <- sort(x)
  ln <- length(x)
  Rank <- seq(1, ln)
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank - 1)/(ln - 1) * Sort_x, na.rm = TRUE)
  b2 <- mean(((Rank - 1) * (Rank - 2))/((ln - 1) * (ln - 2)) * Sort_x, na.rm = TRUE)
  b3 <- mean(((Rank - 1) * (Rank - 2) * (Rank - 3))/((ln - 1) * (ln - 2) * (ln - 3)) * Sort_x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2 * b1 - b0
  L3 <- 6 * b2 - 6 * b1 + b0
  LSKEW <- L3/L2
  class(LSKEW) <- append(class(LSKEW),'LSkew')
  return(LSKEW)
}

