# Parametrs
# Works with different classes
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
  df <- data.frame(Loc, Scale, Shape)
  class(df) <- append(class(df), 'GEVPar')
  return(df)
}



GumbelParams <- function(x, URBEXT2000 = NULL, DeUrb = FALSE, ...){
  if(is(x, 'Ls')){
    Ls <- x 
  } else {
    Ls <- Ls(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  }  
  Scale <- (Ls$L1*Ls$LCV)/log(2)
  Loc <- Ls$L1 - 0.5772*Scale
  Shape <- NA
  df <- data.frame(Loc, Scale, Shape)
  class(df) <- append(class(df), 'GumbelPar')
  return(df)
}


GenLogParams <- function(x, URBEXT2000 = NULL, DeUrb = FALSE, ...){
  if(is(x, 'Ls')){
    Ls <- x 
  } else {
    Ls <- Ls(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  }
  Shape <- -Ls$LSkew
  Scale <- (Ls$L2 * sin(Shape * pi))/(Shape * pi)
  Loc <- Ls$L1 - Scale * ((1/Shape) - (pi/sin(Shape * pi)))
  df <- data.frame(Loc, Scale, Shape)
  class(df) <- append(class(df), 'GenLogPar')
  return(df)
}

GenParetoParams <- function(x, URBEXT2000 = NULL, DeUrb = FALSE, ...){
  if(is(x, 'Ls')){
    Ls <- x 
  } else {
    Ls <- Ls(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  }
  Shape <- (1 - 3 * Ls$LSkew)/(1 + Ls$LSkew)
  Scale <- (1 + Shape) * (2 + Shape) * Ls$L2
  Loc <- Ls$L1 - (2 + Shape) * Ls$L2
  df <- data.frame(Loc, Scale, Shape)
  class(df) <- append(class(df), 'GenParetorPar')
  return(df)
}

GEVParams(Ls(Buildwas))
GEVParams(Buildwas)
GEVParams(Buildwas_Analysis)
GEVParams(Buildwas_Analysis$Hydro_year$Hydro_year_Max)
GumbelParams(Ls(Buildwas))
GumbelParams(Buildwas)
GumbelParams(Buildwas_Analysis)
GumbelParams(Buildwas_Analysis$Hydro_year$Hydro_year_Max)
GenLogParams(Ls(Buildwas))
GenLogParams(Buildwas)
GenLogParams(Buildwas_Analysis)
GenLogParams(Buildwas_Analysis$Hydro_year$Hydro_year_Max)
GenParetoParams(Ls(Buildwas))
GenParetoParams(Buildwas)
GenParetoParams(Buildwas_Analysis)
GenParetoParams(Buildwas_Analysis$Hydro_year$Hydro_year_Max)

Params <- function(x, URBEXT2000 = NULL, DeUrb = FALSE, ...){
  GEV <- GEVParams(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  Gumbel <- GumbelParams(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  GenLog <- GenLogParams(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  GenPareto <- GenParetoParams(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  df <- rbind(GEV, Gumbel, GenLog, GenPareto)
  rownames(df) <- c('GEV', 'Gumbel', 'GenLog', 'GenPareto')
  class(df) <- append(class(df)[1], 'Params')
  return(t(df))
}

Params(Ls(Buildwas, URBEXT2000 = 0.3))

# Growth Factors
GEVGF <- function(x, RP = c(2,4,10,25,50,100), URBEXT2000 = NULL, DeUrb = FALSE, ...) {
  if(is(x, 'Ls')){
    Ls <- x 
  } else {
    Ls <- Ls(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  }
  C <- (2/(3+Ls$LSkew)) - (log(2)/log(3))
  kgev <- 7.859*C+2.95548*C^2
  Bgev <- (kgev*Ls$LCV)/(Ls$LCV*(gamma(1+kgev)-(log(2))^kgev)+gamma(1+kgev)*(1-2^-kgev))
  gf <- 1+(Bgev/kgev)*(log(2)^kgev - (log(RP/(RP-1)))^kgev)
  df <- data.frame(Rerurn_Period = RP, Growth_Factor = gf)
  return(df)
}

GEVGF(Buildwas, URBEXT2000 = 0.3)

GumbelGF <- function(x, RP = c(2,4,10,25,50,100), URBEXT2000 = NULL, DeUrb = FALSE, ...) {
  if(is(x, 'Ls')){
    Ls <- x 
  } else {
    Ls <- Ls(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  }
  B <- Ls$LCV/(log(2)-Ls$LCV*(0.5772+log(log(2))))
  gf <- 1+B*(log(log(2))-log(-log(1-(1/RP))))
  df <- data.frame(Rerurn_Period = RP, Growth_Factor = gf)
  return(df)
}

GumbelGF(Buildwas)

GenLogGF <- function(x, RP = c(2,4,10,25,50,100), URBEXT2000 = NULL, DeUrb = FALSE, ...) {
  if(is(x, 'Ls')){
    Ls <- x 
  } else {
    Ls <- Ls(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  }
  k <- -Ls$LSkew
  B <- Ls$LCV*k*sin((pi)*k)/(k*pi*(k+Ls$LCV)-Ls$LCV*sin((pi)*k))
  gf <- 1+(B/k)*(1-(RP-1)^Ls$LSkew)
  df <- data.frame(Rerurn_Period = RP, Growth_Factor = gf)
  return(df)
}

GenLogGF(Buildwas)

GenParetoGF <- function(x, RP = c(2,4,10,25,50,100), ppy = 1, URBEXT2000 = NULL, DeUrb = FALSE, ...) {
  if(is(x, 'Ls')){
    Ls <- x 
  } else {
    Ls <- Ls(x, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  }
  k <- (1-3*Ls$LSkew)/(1+Ls$LSkew)
  Bgp <- (Ls$LCV*k*(1+k)*(2+k))/(k-Ls$LCV*(2+k)*(2^-k*(1+k)-1))
  RPppy <- 1/((1/RP)/ppy)
  gf <- 1 + (Bgp/k) *((2^-k)-(1-(1-(1/RPppy)))^k)
  df <- data.frame(Rerurn_Period = RP, Growth_Factor = gf)
  return(df)
}

GenParetoGF(Ls(Buildwas))

GrowthFactors <- function(x, RP = c(2,4,10,25,50,100), ppy = 1, URBEXT2000 = NULL, DeUrb = FALSE, ...) {
  GEV <- GEVGF(x, RP = RP, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)[2]
  Gumbel <- GumbelGF(x, RP = RP, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)[2]
  GenLog <- GenLogGF(x, RP = RP, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)[2]
  GenPareto <- GenParetoGF(x, RP = RP, ppy = ppy, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)[2]
  df <- cbind.data.frame(GEV, Gumbel, GenLog, GenPareto)
  rownames(df) <- RP
  colnames(df) <- c('GEV', 'Gumbel', 'GenLog', 'GenPareto')
  class(df) <- append(class(df)[1], 'GFactors')
  return(df)
}

GrowthFactors(Buildwas_Analysis)
GrowthFactors(Ls(Buildwas, URBEXT2000 = 0.3))

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

Estimates(GEVParams(Buildwas))
Estimates(GumbelParams(Buildwas))
Estimates(GenLogParams(Buildwas))
Estimates(GenParetoParams(Buildwas))
Estimates(Buildwas)

