#' @title Volumetric Generalised Extreme Value Growth Factors
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
#' # growthVolGEV.vLs(getVAMAX(buildwas, URBEXT2000 = 0.3))
growthVolGEV <- function(x, ...) {
  UseMethod('growthVolGEV', x)
}

#' @rdname growthVolGEV
#' @export
growthVolGEV.vLs <- function(x, RP =  c(2,4,10,25,50,100,200,1000), URBEXT2000 = NULL, DeUrb = FALSE, ...) {
  lst <- list()
  for(i in seq_along(x$Data)){
    C <- (2 / (3 + x$LSkew[i])) - (log(2) / log(3))
    kgev <- 7.859 * C + 2.95548 * C^2
    Bgev <- (kgev * x$LCV[i]) / (x$LCV[i] * (gamma(1 + kgev) - (log(2))^kgev) +
                                     gamma(1 + kgev) * (1 - 2^-kgev))
    gf <- 1 + (Bgev / kgev) * (log(2)^kgev - (log(RP / (RP - 1)))^kgev)
    lst[[i]] <- gf
  }
  dt <- t(do.call('rbind', lst))
  dt <- as.data.table(dt)
  colnames(dt) <- x$Data
  dt <- data.table(Return_Period = RP, dt)
  return(dt)
}

#' @title Volumetric Gumbel Growth Factors
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
#' # growthVolGumbel.vLs(buildwas, URBEXT2000 = 0.3)
growthVolGumbel <- function(x, ...) {
  UseMethod('growthVolGumbel', x)
}

#' @rdname growthVolGumbel
#' @export
growthVolGumbel.vLs <- function(x, RP =  c(2,4,10,25,50,100,200,1000), URBEXT2000 = NULL, DeUrb = FALSE, ...) {
  lst <- list()
  for(i in seq_along(x$Data)){
    B <- x$LCV[i]/(log(2)-x$LCV[i]*(0.5772+log(log(2))))
    gf <- 1+B*(log(log(2))-log(-log(1-(1/RP))))
    lst[[i]] <- gf
  }
  dt <- t(do.call('rbind', lst))
  dt <- as.data.table(dt)
  colnames(dt) <- x$Data
  dt <- data.table(Return_Period = RP, dt)
  return(dt)
}

#' @title Volumetric Generalised Logistic Growth Factors
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
#' # growthVolGenLog(buildwas, URBEXT2000 = 0.3)
growthVolGenLog <- function(x, ...) {
  UseMethod('growthVolGenLog', x)
}

#' @rdname growthVolGenLog
#' @export
growthVolGenLog.vLs <- function(x, RP =  c(2,4,10,25,50,100,200,1000), URBEXT2000 = NULL, DeUrb = FALSE, ...) {
  lst <- list()
  for(i in seq_along(x$Data)){
    k <- -x$LSkew[i]
    B <- x$LCV[i] * k * sin((pi) * k) / (k * pi * (k + x$LCV[i]) - x$LCV[i] * sin((pi) * k))
    gf <- 1 + (B/k) * (1-(RP-1)^x$LSkew[i])
    lst[[i]] <- gf
  }
  dt <- t(do.call('rbind', lst))
  dt <- as.data.table(dt)
  colnames(dt) <- x$Data
  dt <- data.table(Return_Period = RP, dt)
  return(dt)
}

#' @title Volumetric Generalised Pareto Growth Factors
#'
#' @param x Dataset of HydroEnR classes or L-moments
#' @param RP Vector of required return periods
#' @param ppy Peaks per year
#' @param URBEXT2000 If you wish to add urban adjustment the L-moments
#' @param DeUrb Set to FALSE, set to TRUE should you want to deurbanise
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' # growthVolGenPareto(Ls(buildwas))
growthVolGenPareto <- function(x, ...) {
  UseMethod('growthVolGenPareto', x)
}

#' @rdname growthVolGenPareto
#' @export
growthVolGenPareto.vLs <- function(x, RP =  c(2,4,10,25,50,100,200,1000), ppy = 1, URBEXT2000 = NULL, DeUrb = FALSE, ...) {
  lst <- list()
  for(i in seq_along(x$Data)){
    k <- (1-3*x$LSkew[i])/(1+x$LSkew[i])
    Bgp <- (x$LCV[i]*k*(1+k)*(2+k))/(k-x$LCV[i]*(2+k)*(2^-k*(1+k)-1))
    RPppy <- 1/((1/RP)/ppy)
    gf <- 1 + (Bgp/k) *((2^-k)-(1-(1-(1/RPppy)))^k)
    lst[[i]] <- gf
  }
  dt <- t(do.call('rbind', lst))
  dt <- as.data.table(dt)
  colnames(dt) <- x$Data
  dt <- data.table(Return_Period = RP, dt)
  return(dt)
}

#' @title Calculate the growth factors for all methods
#'
#' @param x Dataset of HydroEnR classes or L-moments
#' @param RP Vector of required return periods
#' @param ppy Peaks per year
#' @param URBEXT2000 If you wish to add urban adjustment the L-moments
#' @param DeUrb Set to FALSE, set to TRUE should you want to deurbanise
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' # growthFactors(buildwas_analysis)
#' # growthFactors(Ls(buildwas))
growthVolFactors <- function(x, RP = c(2,4,10,25,50,100,200,1000), ppy = 1, URBEXT2000 = NULL, DeUrb = FALSE, ...) {
    GEV <- growthVolGEV(x, RP = RP, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
    Gumbel <- growthVolGumbel(x, RP = RP, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
    GenLog <- growthVolGenLog(x, RP = RP, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
    GenPareto <- growthVolGenPareto(x, RP = RP, ppy = ppy, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
    Distribution <- rep(c('GEV','Gumbel','GenLog','GenPareto'), each = length(RP))
    dt <- data.table(Distribution, rbind(GEV, Gumbel, GenLog, GenPareto))
  # dt <- data.table(Return = RP,
  #                  GEV = GEV$Growth_Factor,
  #                  Gumbel = Gumbel$Growth_Factor,
  #                  GenLog = GenLog$Growth_Factor,
  #                  GenPareto = GenPareto$Growth_Factor)
  class(dt) <- append(class(dt), 'volGFactors')
  return(dt)
}
