
#' @title Return Period Estimate
#'
#' @description  Generate a flow estimate based on a user defined return period.
#' Alternatively add an observed flow to provide an estimate of return period.
#'
#' @param x Parameters defined from various other functions
#' @param q An observed flow value, set to NULL but use to find return period
#' @param RP Set to 100, return period that you wish to generate a flow estimate for
#' @param ppy Peaks per year, set as one for Gen Pareto only
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' Estimates(GEVParams(buildwas), RP = c(100,200))
#' Estimates(GumbelParams(buildwas))
#' Estimates(GenLogParams(buildwas_max), q = 650)
#' Estimates(GenParetoParams(buildwas), ppy =2)
Estimates <- function(x, q, RP, ppy, ...){
  UseMethod('Estimates', x)
}

#' @rdname Estimates
#' @export
Estimates.GEVPar <- function (x, q = NULL, RP = 100){
  if (is.null(q) == TRUE) {
    res <- x[1] + x[2]/x[3] * (1 - (-log(1 - 1/RP))^x[3])
  }
  else {
    y <- -x[3]^(-1) * log(1 - x[3] * (q - x[1])/x[2])
    P <- 1 - (exp(-exp(-y)))
    res <- 1/P
  }
  return(res)
}

#' @rdname Estimates
#' @export
Estimates.volGEVPar <- function (x, q = NULL, RP = 100){
  lst <- list()
  for(i in seq_along(x$Data)){
    if (is.null(q) == TRUE){
      res <- x$Loc[1] + x$Scale[i]/x$Shape[i] * (1 - (-log(1 - 1/RP))^x$Shape[i])
      type <- 'RP'
      vals <- RP
      len <- length(RP)
      lst[[i]] <- data.table(res)
    }else{
      y <- -x$Scale[i]^(-1) * log(1 - x$Scale[i] * (q - x$Loc[i])/x$Scale[i])
      P <- 1 - (exp(-exp(-y)))
      res <- 1/P
      type <- 'q'
      vals <- q
      len <- length(q)
      lst[[i]] <- data.table(res)
    }
  }

  IDs <- rep(paste(type, RP, sep = '_'), each = len)
  dt <- data.table(x$Data, IDs, rbindlist(lst))
  colnames(dt) <- c('Data', 'ID', 'type')
  return(dt)
}


#' @rdname Estimates
#' @export
Estimates.GumbelPar <- function(x, q = NULL, RP = 100){
  if(is.null(q) == TRUE) {res <- x[1]+x[2]*(-log(-log(1-(1/RP))))}
  else {
    Prob <- 1- exp(-exp(-(q - x[1])/x[2]))
    res <- 1/Prob}
  return(res)
}

#' @rdname Estimates
#' @export
Estimates.GenLogPar <- function(x, q = NULL, RP = 100){
  if(is.null(q) == TRUE) {
    res <- x[1] + x[2]/x[3] * (1 - (RP - 1)^-x[3])
  }
  else {
    y <- -x[3]^(-1) * log(1 - x[3] * (q - x[1])/x[2])
    P <- 1 - (1/(1 + exp(-y)))
    res <- 1/P
  }
  return(res)
}

#' @rdname Estimates
#' @export
Estimates.GenParetoPar <- function(x, q = NULL, RP = 100, ppy = 1){
  if (is.null(q) == TRUE) {
    res <- x[1] + x[2] * (1 - (1 - (1 - (1/RP)/ppy))^x[3])/x[3]
  }
  else {
    y <- -x[3]^-1 * log(1 - x[3] * (q - loc)/x[2])
    P <- 1 - (1 - exp(-y))
    RPPOT <- 1/P
    res <- RPPOT/ppy
  }
  return(res)
}


