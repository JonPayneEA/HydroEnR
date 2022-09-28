#' @title Determine parameters of Generalised Extreme Value fit for volumetric data
#'
#' @param x Dataset of HydroEnR classes or vL-moments
#' @param RP Vector of required return periods
#' @param URBEXT2000 If you wish to add urban adjustment the L-moments
#' @param DeUrb Set to FALSE, set to TRUE should you want to deurbanise
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' #volGEVParams(buildwas)
volGEVParams <- function(x, ...) {
  UseMethod('volGEVParams', x)
}

#' @rdname volGEVParams
#' @export
volGEVParams.vLs <- function(x, URBEXT2000 = NULL, DeUrb = FALSE, ...){
  lst <- list()
  for(i in seq_along(x$Data)){
    C <- (2/(3+x$LSkew[i])) - (log(2)/log(3))
    Shape <- 7.859*C+2.95548*C^2
    Scale <- (x$L2[i]*Shape)/((1-2^-Shape)*gamma(1+Shape))
    Loc <- x$L1[i]-Scale*((1-gamma(1+Shape))/Shape)
    dt <- data.table(Loc, Scale, Shape)
    lst[[i]] <- dt
  }
  dt <- data.table(Data = x$Data, rbindlist(lst))
  class(dt) <- append(class(dt), 'volGEVPar')
  return(dt)
}

#' @title Determine parameters of Gumbel fit for volumetric data
#'
#' @param x Dataset of HydroEnR classes or vL-moments
#' @param RP Vector of required return periods
#' @param URBEXT2000 If you wish to add urban adjustment the L-moments
#' @param DeUrb Set to FALSE, set to TRUE should you want to deurbanise
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' #volGumbelParams(buildwas)
volGumbelParams <- function(x, ...) {
  UseMethod('volGumbelParams', x)
}

#' @rdname volGumbelParams
#' @export
volGumbelParams.vLs <- function(x, URBEXT2000 = NULL, DeUrb = FALSE, ...){
  lst <- list()
  for(i in seq_along(x$Data)){
    Scale <- (x$L1[i]*x$LCV[i])/log(2)
    Loc <- x$L1[i] - 0.5772*Scale
    Shape <- NA
    dt <- data.table(Loc, Scale, Shape)
    lst[[i]] <- dt
  }
  dt <- data.table(Data = x$Data, rbindlist(lst))
  class(dt) <- append(class(dt), 'volGumbelPar')
  return(dt)
}

#' @title Determine parameters of Generalised Logistic fit for volumetric data
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
#' #volGenLogParams(buildwas)
volGenLogParams <- function(x, ...) {
  UseMethod('volGenLogParams', x)
}

#' @rdname volGenLogParams
#' @export
volGenLogParams.vLs <- function(x, URBEXT2000 = NULL, DeUrb = FALSE, ...){
  lst <- list()
  for(i in seq_along(x$Data)){
    Shape <- -x$LSkew[i]
    Scale <- (x$L2[i] * sin(Shape * pi))/(Shape * pi)
    Loc <- x$L1[i] - Scale * ((1/Shape) - (pi/sin(Shape * pi)))
    dt <- data.table(Loc, Scale, Shape)
    lst[[i]] <- dt
  }
  dt <- data.table(Data = x$Data, rbindlist(lst))
  class(dt) <- append(class(dt), 'volGenLogPar')
  return(dt)
}

#' @title Determine parameters of Generalised Pareto fit for volumetric data
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
#' #volGenParetoParams(buildwas)
volGenParetoParams <- function(x, ...) {
  UseMethod('volGenParetoParams', x)
}

#' @rdname volGenParetoParams
#' @export
volGenParetoParams.vLs <- function(x, URBEXT2000 = NULL, DeUrb = FALSE, ...){
  lst <- list()
  for(i in seq_along(x$Data)){
    Shape <- (1 - 3 * x$LSkew[i])/(1 + x$LSkew[i])
    Scale <- (1 + Shape) * (2 + Shape) * x$L2[i]
    Loc <- x$L1[i] - (2 + Shape) * x$L2[i]
    dt <- data.table(Loc, Scale, Shape)
    lst[[i]] <- dt
  }
  dt <- data.table(Data = x$Data, rbindlist(lst))
  class(dt) <- append(class(dt), 'volGenParetoPar')
  return(dt)
}

#' @title Determine parameters of all available model fits for volumetric data
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
volParams <- function(x, URBEXT2000 = NULL, DeUrb = FALSE, ...){
  GEV <- volGEVParams(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  Gumbel <- volGumbelParams(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  GenLog <- volGenLogParams(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  GenPareto <- volGenParetoParams(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  Distribution <- rep(c('GEV','Gumbel','GenLog','GenPareto'), each = length(x$Data))
  dt <- data.table(Distribution, rbind(GEV, Gumbel, GenLog, GenPareto))
  class(dt) <- append(class(dt), 'volGFactors')
  return(dt)
}

