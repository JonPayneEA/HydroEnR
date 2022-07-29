#' @description Provides an urban adjustment to the LCV and LSKEW moments.
#'
#' @title urbAdj
#'
#' @param x Data of class Lcv or LSkew
#' @param URBEXT2000 Value taken from the catchment descriptors
#' @param DeUrb Set to false. If true, the data are de-urbanised
#' @param ... Additional parameters that can be added
#'
#' @return
#' @export
#'
#' @examples
#' flow %>%
#'  getAMAX() %>%
#'  Lcv() %>%
#'  urbAdj(URBEXT2000 = 0.3, DeUrb = FALSE)
#' @rdname urbAdj
#' @export
urbAdj <- function(x, URBEXT2000, DeUrb = FALSE, ...) {
  UseMethod('urbAdj', x)
}


#' @rdname urbAdj
#' @export
urbAdj.Lcv<- function (x, URBEXT2000, DeUrb = FALSE){
  if (DeUrb == FALSE) {
    x * 0.68654^(1.567 * URBEXT2000)
  }
  else {
    x/(0.68654^(1.567 * URBEXT2000))
  }
}

#' @rdname urbAdj
#' @export
urbAdj.LSkew <- function (x, URBEXT2000, DeUrb = FALSE){
  if (DeUrb == FALSE) {
    ((x + 1) * 1.096017^(1.567 * URBEXT2000)) - 1
  }
  else {
    ((x + 1)/1.096017^(1.567 * URBEXT2000)) - 1
  }
}

