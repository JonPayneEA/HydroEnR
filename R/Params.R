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
