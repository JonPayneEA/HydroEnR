#' @title L moments
#'
#' @description Calculates the L moments from AMAX data. Can also convert time
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
#' Ls(data)
Ls <- function(x, URBEXT2000 = NULL, DeUrb = FALSE, ...) {
  L1 <- L1(x)
  L2 <- L2(x)
  L3 <- L3(x)
  L4 <- L4(x)
  LCV <- LCV(x)
  LCA <- LCA(x)
  LKUR <- LKur(x)
  LSkew <- LSKEW(x)
  QMED <- QMED(x)
  if(!is.null(URBEXT2000)){
    LCV <- urbAdj(LCV, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
    LSkew <- urbAdj(LSkew, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  }
  x <- data.frame(L1, L2, L3, L4, LCV, LCA, LKUR, LSkew, QMED)
  class(x) <- append(class(x), 'Ls')
  return(x)
}
