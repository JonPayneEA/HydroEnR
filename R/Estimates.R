# Estimates
Estimates.GEVPar <- function (x, q = NULL, RP = 100)
{
  if (is.null(q) == TRUE) {
    res <- x$Loc + x$Scale/x$Shape * (1 - (-log(1 - 1/RP))^x$Shape)
  }
  else {
    y <- -x$Shape^(-1) * log(1 - x$Shape * (q - x$Loc)/x$Scale)
    P <- 1 - (exp(-exp(-y)))
    res <- 1/P
  }
  return(res)
}


Estimates.GumbelPar <- function(x, q = NULL, RP = 100){
  if(is.null(q) == TRUE) {res <- x$Loc+x$Scale*(-log(-log(1-(1/RP))))}
  else {
    Prob <- 1- exp(-exp(-(q - x$Loc)/scale))
    res <- 1/Prob}
  return(res)
}


Estimates.GenLogPar <- function(x, q = NULL, RP = 100){
  if(is.null(q) == TRUE) {
    res <- x$Loc + x$Scale/x$Shape * (1 - (RP - 1)^-x$Shape)
  }
  else {
    y <- -x$Shape^(-1) * log(1 - shape * (q - x$Loc)/x$Scale)
    P <- 1 - (1/(1 + exp(-y)))
    res <- 1/P
  }
  return(res)
}


Estimates.GenParetorPar <- function(x, q = NULL, RP = 100, ppy = 1){
  if (is.null(q) == TRUE) {
    res <- x$Loc + x$Scale * (1 - (1 - (1 - (1/RP)/ppy))^x$Shape)/x$Shape
  }
  else {
    y <- -x$Shape^-1 * log(1 - x$Shape * (q - loc)/x$Scale)
    P <- 1 - (1 - exp(-y))
    RPPOT <- 1/P
    res <- RPPOT/ppy
  }
  return(res)
}

# Estimates.Params <- function(x, q = NULL, RP = 100, ppy = 1){
#   GEV <- x[1,]
#   class(GEV) <- append(class(GEV)[1], 'GEVPar')
#   GEV <- Estimates(GEV, RP = RP)
#   Gumbel <- x[2,]
#   class(Gumbel) <- append(class(Gumbel)[1], 'GumbelPar')
#   Gumbel <- Estimates(Gumbel, RP = RP)
#   GenLog <- x[3,]
#   class(GenLog) <- append(class(GenLog)[1], 'GenLogPar')
#   GenLog <- Estimates(GenLog, RP = RP)
#   GenPareto <- x[4,]
#   class(GenPareto) <- append(class(GenPareto)[1], 'GenParetorPar')
#   GenPareto <- Estimates(GenPareto, RP = RP, q = q, ppy = ppy)
#   df <- cbind.data.frame(GEV, Gumbel, GenLog, GenPareto)
#   colnames(df) <- c('GEV', 'Gumbel', 'GenLog', 'GenPareto')
#   rownames(df) <- RP
#   class(df) <- append(class(df)[1], 'RPs')
#   return(df)
# }
#
#
# Estimates.Params(Buildwas)

Estimates <- function(x, ...){
  UseMethod('Estimates', x)
}

# Estimates(GEVParams(Buildwas))
# Estimates(GumbelParams(Buildwas))
# Estimates(GenLogParams(Buildwas))
# Estimates(GenParetoParams(Buildwas))
# Estimates(Buildwas)

