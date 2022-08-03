#' @title L1 moment
#'
#' @description Calculates the L1 moment from AMAX data. Can also convert time
#' series data imported via HydroEnR into L1 moments.
#'
#' @param x AMAX or flow time series
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' data <- getAMAX(rnrfa::get_ts(id = 2001, type = 'amax-flow'))
#' L1(data)
L1 <- function(x, ...) {
  UseMethod('L1', x)
}

#' @rdname L1
#' @export
L1.numeric <- function(x, ...) {
  x <- mean(x, na.rm = TRUE)
  class(x) <- append(class(x), 'L1')
  return(x)
}

#' @rdname L1
#' @export
L1.HydroAggsmax <- function(x, ...){
  x <- mean(x$Hydro_year$Hydro_year_Max, na.rm = TRUE)
  class(x) <- append(class(x), 'L1')
  return(x)
}

#' @rdname L1
#' @export
L1.HydroAMAX <- function(x, ...){
  x <- mean(x$AMAX, na.rm = TRUE)
  class(x) <- append(class(x), 'L1')
  return(x)
}

#' @rdname L1
#' @export
L1.flowLoad <- function(x, ...){
  x <- getAMAX(x)
  x <- mean(x$AMAX, na.rm = TRUE)
  class(x) <- append(class(x), 'L1')
  return(x)
}

#' @rdname L1
#' @export
# Fixing the print of L1 class data
print.L1 <- function(x, ...) {
  attr(x, "class") <- NULL
  print.default(x, ...)
}


