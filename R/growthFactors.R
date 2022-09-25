# Growth Factors

#' @title Generalised Extreme Value Growth Factors
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
#' # GEVGF(Buildwas, URBEXT2000 = 0.3)
GEVGF <- function(x, RP =  c(2,4,10,25,50,100,200,1000), URBEXT2000 = NULL, DeUrb = FALSE, ...) {
  if(is(x, 'Ls')){
    Ls <- x
  } else {
    Ls <- Ls(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  }
  C <- (2 / (3 + Ls$LSkew)) - (log(2) / log(3))
  kgev <- 7.859 * C + 2.95548 * C^2
  Bgev <- (kgev * Ls$LCV) / (Ls$LCV * (gamma(1 + kgev) - (log(2))^kgev) +
                               gamma(1 + kgev) * (1 - 2^-kgev))
  gf <- 1 + (Bgev / kgev) * (log(2)^kgev - (log(RP / (RP - 1)))^kgev)
  df <- data.table(Return_Period = RP, Growth_Factor = gf)
  return(df)
}

#' @title Gumbel Growth Factors
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
#' # GumbelGF(buildwas, URBEXT2000 = 0.3)
GumbelGF <- function(x, RP =  c(2,4,10,25,50,100,200,1000), URBEXT2000 = NULL, DeUrb = FALSE, ...) {
  if(is(x, 'Ls')){
    Ls <- x
  } else {
    Ls <- Ls(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  }
  B <- Ls$LCV/(log(2)-Ls$LCV*(0.5772+log(log(2))))
  gf <- 1+B*(log(log(2))-log(-log(1-(1/RP))))
  df <- data.table(Return_Period = RP, Growth_Factor = gf)
  return(df)
}

#' @title Generalised Logistic Growth Factors
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
#' # GenLogGF(Buildwas, URBEXT2000 = 0.3)
GenLogGF <- function(x, RP =  c(2,4,10,25,50,100,200,1000), URBEXT2000 = NULL, DeUrb = FALSE, ...) {
  if(is(x, 'Ls')){
    Ls <- x
  } else {
    Ls <- Ls(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  }
  k <- -Ls$LSkew
  B <- Ls$LCV * k * sin((pi) * k) / (k * pi * (k + Ls$LCV) - Ls$LCV * sin((pi) * k))
  gf <- 1 + (B/k) * (1-(RP-1)^Ls$LSkew)
  df <- data.table(Return_Period = RP, Growth_Factor = gf)
  return(df)
}

#' @title Generalised Pareto Growth Factors
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
#' # GenParetoGF(Ls(buildwas))
GenParetoGF <- function(x, RP =  c(2,4,10,25,50,100,200,1000), ppy = 1, URBEXT2000 = NULL, DeUrb = FALSE, ...) {
  if(is(x, 'Ls')){
    Ls <- x
  } else {
    Ls <- Ls(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  }
  k <- (1-3*Ls$LSkew)/(1+Ls$LSkew)
  Bgp <- (Ls$LCV*k*(1+k)*(2+k))/(k-Ls$LCV*(2+k)*(2^-k*(1+k)-1))
  RPppy <- 1/((1/RP)/ppy)
  gf <- 1 + (Bgp/k) *((2^-k)-(1-(1-(1/RPppy)))^k)
  df <- data.table(Return_Period = RP, Growth_Factor = gf)
  return(df)
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
growthFactors <- function(x, RP = c(2,4,10,25,50,100,200,1000), ppy = 1, URBEXT2000 = NULL, DeUrb = FALSE, ...) {
  GEV <- GEVGF(x, RP = RP, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)[,2]
  Gumbel <- GumbelGF(x, RP = RP, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)[,2]
  GenLog <- GenLogGF(x, RP = RP, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)[,2]
  GenPareto <- GenParetoGF(x, RP = RP, ppy = ppy, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)[,2]
  dt <- data.table(Return = RP,
                   GEV = GEV$Growth_Factor,
                   Gumbel = Gumbel$Growth_Factor,
                   GenLog = GenLog$Growth_Factor,
                   GenPareto = GenPareto$Growth_Factor)
  class(dt) <- append(class(dt)[1], 'GFactors')
  return(dt)
}
