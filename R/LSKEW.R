# LSkew

LSKEW.default <- function(x){
  Sort_x <- sort(x)
  ln <- length(x)
  Rank <- seq(1, ln)
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank - 1)/(ln - 1) * Sort_x, na.rm = TRUE)
  b2 <- mean(((Rank - 1) * (Rank - 2))/((ln - 1) * (ln - 2)) * Sort_x, na.rm = TRUE)
  b3 <- mean(((Rank - 1) * (Rank - 2) * (Rank - 3))/((ln - 1) * (ln - 2) * (ln - 3)) * Sort_x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2 * b1 - b0
  L3 <- 6 * b2 - 6 * b1 + b0
  LSKEW <- L3/L2
  class(LSKEW) <- append(class(LSKEW),'LSkew')
  return(LSKEW)
}

LSKEW.HydroAggsmax <- function(x){
  x <- x$Hydro_year$HydroYear_Max
  Sort_x <- sort(x)
  ln <- length(x)
  Rank <- seq(1, ln)
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank - 1)/(ln - 1) * Sort_x, na.rm = TRUE)
  b2 <- mean(((Rank - 1) * (Rank - 2))/((ln - 1) * (ln - 2)) * Sort_x, na.rm = TRUE)
  b3 <- mean(((Rank - 1) * (Rank - 2) * (Rank - 3))/((ln - 1) * (ln - 2) * (ln - 3)) * Sort_x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2 * b1 - b0
  L3 <- 6 * b2 - 6 * b1 + b0
  LSKEW <- L3/L2
  class(LSKEW) <- append(class(LSKEW),'LSkew')
  return(LSKEW)
}

LSKEW(Buildwas_Analysis)

LSKEW.HydroAMAX <- function(x){
  x <- x$AMAX
  Sort_x <- sort(x)
  ln <- length(x)
  Rank <- seq(1, ln)
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank - 1)/(ln - 1) * Sort_x, na.rm = TRUE)
  b2 <- mean(((Rank - 1) * (Rank - 2))/((ln - 1) * (ln - 2)) * Sort_x, na.rm = TRUE)
  b3 <- mean(((Rank - 1) * (Rank - 2) * (Rank - 3))/((ln - 1) * (ln - 2) * (ln - 3)) * Sort_x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2 * b1 - b0
  L3 <- 6 * b2 - 6 * b1 + b0
  LSKEW <- L3/L2
  class(LSKEW) <- append(class(LSKEW),'LSkew')
  return(LSKEW)
}

LSKEW.FlowLoad <- function(x){
  x <- GetAMAX(x)
  x <- x$AMAX
  Sort_x <- sort(x)
  ln <- length(x)
  Rank <- seq(1, ln)
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank - 1)/(ln - 1) * Sort_x, na.rm = TRUE)
  b2 <- mean(((Rank - 1) * (Rank - 2))/((ln - 1) * (ln - 2)) * Sort_x, na.rm = TRUE)
  b3 <- mean(((Rank - 1) * (Rank - 2) * (Rank - 3))/((ln - 1) * (ln - 2) * (ln - 3)) * Sort_x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2 * b1 - b0
  L3 <- 6 * b2 - 6 * b1 + b0
  LSKEW <- L3/L2
  class(LSKEW) <- append(class(LSKEW),'LSkew')
  return(LSKEW)
}

LSKEW <- function(x,...) {
  UseMethod('LSKEW', x)
}

# Fixing the print of LSkew class data
print.LSkew <- function(x, ...) {
  attr(x, "class") <- NULL
  print.default(x, ...)
}

LSKEW(Buildwas)
LSKEW(Buildwas_Analysis)
Urb(LSKEW(Buildwas_Analysis), 0.3)
LSKEW(Buildwas_Analysis$Hydro_year$HydroYear_Max) %>% Urb(0.3)
LSKEW(GetAMAX(rnrfa::get_ts(id = 2001, type = 'amax-flow')))
