#' @title Determine parameters of Generalised Extreme Value fit
#'
#' @param x Dataset of HydroEnR classes or L-moments
#' @param RP Vector of required return periods
#' @param URBEXT2000 If you wish to add urban adjustment the L-moments
#' @param DeUrb Set to FALSE, set to TRUE should you want to deurbanise
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' #GEVParams(buildwas)
GEVParams <- function(x, URBEXT2000 = NULL, DeUrb = FALSE, ...){
  if(is(x, 'Ls')){
    Ls <- x
  } else {
    Ls <- Ls(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  }
  C <- (2/(3+Ls$LSkew)) - (log(2)/log(3))
  Shape <- 7.859*C+2.95548*C^2
  Scale <- (Ls$L2*Shape)/((1-2^-Shape)*gamma(1+Shape))
  Loc <- Ls$L1-Scale*((1-gamma(1+Shape))/Shape)
  dt <- t(data.frame(Loc, Scale, Shape))
  colnames(dt) <- 'GEV'
  class(dt) <- append(class(dt), 'GEVPar')
  return(dt)
}

#' @title Determine parameters of Gumbel fit
#'
#' @param x Dataset of HydroEnR classes or L-moments
#' @param RP Vector of required return periods
#' @param URBEXT2000 If you wish to add urban adjustment the L-moments
#' @param DeUrb Set to FALSE, set to TRUE should you want to deurbanise
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' #GumbelParams(buildwas)
GumbelParams <- function(x, URBEXT2000 = NULL, DeUrb = FALSE, ...){
  if(is(x, 'Ls')){
    Ls <- x
  } else {
    Ls <- Ls(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  }
  Scale <- (Ls$L1*Ls$LCV)/log(2)
  Loc <- Ls$L1 - 0.5772*Scale
  Shape <- NA
  dt <- t(data.frame(Loc, Scale, Shape))
  colnames(dt) <- 'Gumbel'
  class(dt) <- append(class(dt), 'GumbelPar')
  return(dt)
}

#' @title Determine parameters of Generalised Logistic fit
#'
#' @param x Dataset of HydroEnR classes or L-moments
#' @param RP Vector of required return periods
#' @param URBEXT2000 If you wish to add urban adjustment the L-moments
#' @param DeUrb Set to FALSE, set to TRUE should you want to deurbanise
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' #GenLogParams(buildwas)
GenLogParams <- function(x, URBEXT2000 = NULL, DeUrb = FALSE, ...){
  if(is(x, 'Ls')){
    Ls <- x
  } else {
    Ls <- Ls(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  }
  Shape <- -Ls$LSkew
  Scale <- (Ls$L2 * sin(Shape * pi))/(Shape * pi)
  Loc <- Ls$L1 - Scale * ((1/Shape) - (pi/sin(Shape * pi)))
  dt <- t(data.frame(Loc, Scale, Shape))
  colnames(dt) <- 'GenLog'
  class(dt) <- append(class(dt), 'GenLogPar')
  return(dt)
}

#' @title Determine parameters of Generalised Pareto fit
#'
#' @param x Dataset of HydroEnR classes or L-moments
#' @param RP Vector of required return periods
#' @param URBEXT2000 If you wish to add urban adjustment the L-moments
#' @param DeUrb Set to FALSE, set to TRUE should you want to deurbanise
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' #GenParetoParams(buildwas)
GenParetoParams <- function(x, URBEXT2000 = NULL, DeUrb = FALSE, ...){
  if(is(x, 'Ls')){
    Ls <- x
  } else {
    Ls <- Ls(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  }
  Shape <- (1 - 3 * Ls$LSkew)/(1 + Ls$LSkew)
  Scale <- (1 + Shape) * (2 + Shape) * Ls$L2
  Loc <- Ls$L1 - (2 + Shape) * Ls$L2
  dt <- t(data.frame(Loc, Scale, Shape))
  colnames(dt) <- 'GenPareto'
  class(dt) <- append(class(dt), 'GenParetoPar')
  return(dt)
}

#' @title Determine parameters of all available model fits
#'
#' @param x Dataset of HydroEnR classes or L-moments
#' @param RP Vector of required return periods
#' @param URBEXT2000 If you wish to add urban adjustment the L-moments
#' @param DeUrb Set to FALSE, set to TRUE should you want to deurbanise
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' #Params(buildwas)
Params <- function(x, URBEXT2000 = NULL, DeUrb = FALSE, ...){
  GEV <- GEVParams(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  Gumbel <- GumbelParams(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  GenLog <- GenLogParams(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  GenPareto <- GenParetoParams(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  dt <- data.table(Parameter = rownames(GEV),
                   GEV,
                   Gumbel,
                   GenLog,
                   GenPareto)
  class(dt) <- append(class(dt)[1], 'Params')
  return(dt)
}
