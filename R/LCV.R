#' @title LCV moment
#'
#' @description Calculates the LCV moment from AMAX data. Can also convert time
#' series data imported via HydroEnR into LCV moments.
#'
#' @param x AMAX or flow time series
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' data <- getAMAX(rnrfa::get_ts(id = 2001, type = 'amax-flow'))
#' LCV(data)
LCV <- function(x, ...) {
  UseMethod('LCV', x)
}

#' @rdname LCV
#' @export
LCV.numeric <- function(x) {
  Sort_x <- sort(x)
  ln <- length(x)
  Rank <- seq(1, ln)
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank - 1)/(ln - 1) * Sort_x, na.rm = TRUE)
  b2 <- mean(((Rank - 1) * (Rank - 2))/((ln - 1) * (ln - 2)) * Sort_x, na.rm = TRUE)
  b3 <- mean(((Rank - 1) * (Rank - 2) * (Rank - 3))/((ln - 1) * (ln - 2) * (ln - 3)) * Sort_x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2 * b1 - b0
  LCV <- L2/L1
  class(LCV) <- append(class(LCV), 'Lcv')
  return(LCV)
}

#' @rdname LCV
#' @export
LCV.HydroAggsmax <- function(x){
  x <- x$Hydro_year$Hydro_year_Max
  Sort_x <- sort(x)
  ln <- length(x)
  Rank <- seq(1, ln)
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank - 1)/(ln - 1) * Sort_x, na.rm = TRUE)
  b2 <- mean(((Rank - 1) * (Rank - 2))/((ln - 1) * (ln - 2)) * Sort_x, na.rm = TRUE)
  b3 <- mean(((Rank - 1) * (Rank - 2) * (Rank - 3))/((ln - 1) * (ln - 2) * (ln - 3)) * Sort_x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2 * b1 - b0
  LCV <- L2/L1
  class(LCV) <- append(class(LCV), 'Lcv')
  return(LCV)
}

#' @rdname LCV
#' @export
LCV.HydroAMAX <- function(x){
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
  LCV <- L2/L1
  class(LCV) <- append(class(LCV), 'Lcv')
  return(LCV)
}

#' @rdname LCV
#' @export
LCV.flowLoad <- function(x){
  x <- getAMAX(x)
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
  LCV <- L2/L1
  class(LCV) <- append(class(LCV), 'Lcv')
  return(LCV)
}

#' @rdname LCV
#' @export
print.Lcv <- function(x, ...) {
  # Fixing the print of Lcv class data
  attr(x, "class") <- NULL
  print.default(x, ...)
}

# LCV(Buildwas)
# LCV(Buildwas_Analysis)
# LCV(Buildwas_Analysis) %>% Urb(0.3)
# LCV(Buildwas_Analysis) %>% urb(0.3)
# LCV(Buildwas_Analysis$Hydro_year$Hydro_year_Max)
# LCV(GetAMAX(rnrfa::get_ts(id = 2001, type = 'amax-flow')))


