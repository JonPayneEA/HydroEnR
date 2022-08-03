#' @title Generate a QMED estimate
#'
#' @description  Calculate the QMED value of a series of flow data loaded into
#' the environment via HydroEnR.
#'
#' @param x A series of flow data loaded into the environment via HydroEnR
#' @param ... Additional parameters
#'
#' @return
#' @export
#'
#' @examples
#' QMED(Buildwas)
#' QMED(Buildwas_Analysis)

QMED <- function(x,...) {
  UseMethod('QMED', x)
}

#' @rdname QMED
#' @export
QMED.numeric <- function(x, ...) {
  x <- median(x, na.rm = TRUE)
  class(x) <- append(class(x), 'QMED')
  return(x)
}

#' @rdname QMED
#' @export
QMED.HydroAggsmax <- function(x, ...){
  x <- median(x$Hydro_year$Hydro_year_Max, na.rm = TRUE)
  class(x) <- append(class(x), 'QMED')
  return(x)
}

#' @rdname QMED
#' @export
QMED.HydroAMAX <- function(x, ...){
  x <- median(x$AMAX, na.rm = TRUE)
  class(x) <- append(class(x), 'QMED')
  return(x)
}

#' @rdname QMED
#' @export
QMED.flowLoad <- function(x, ...){
  x <- getAMAX(x)
  x <- median(x$AMAX, na.rm = TRUE)
  class(x) <- append(class(x), 'QMED')
  return(x)
}

#' @rdname QMED
#' @export
QMED.zoo <- function(x, ...){
  x <- getAMAX(x)
  x <- median(x$AMAX, na.rm = TRUE)
  class(x) <- append(class(x), 'QMED')
  return(x)
}

#' @rdname QMED
#' @description Integrates a fix for the print function
#' @export
# Fixing the print of Lcv class data
print.QMED <- function(x, ...) {
  attr(x, "class") <- NULL
  print.default(x, ...)
}




