#' Optimise rating relationships on multiple limbs
#'
#' @description Optimises the rating relationships using the Levenberg-Marquardt algorithm
#'
#' @param Discharge Flow series
#' @param Stage Level/stage series
#' @param control Change points in the rating
#' @param ...  Apply additional commands to the Non-linear Least Squares optimiser
#'
#' @return
#' @export
#'
#' @examples
#' library(data.table)
#' library(minpack.lm)
#' gaugings <- data.frame(Stage = c(1.855, 2.109, 2.037, 1.972, 2.574, 1.748, 2.016),
#'                        Discharge = c(177.685, 240.898, 221.954, 205.55, 383.051, 154.061, 216.582))
#'
#' # Writing data into an R dataframe:
#' gaugings <- data.frame(stage=c(0.1374976,0.1354978,0.1547908,0.1547908,0.3047804,0.3022808,0.4138614,0.2901140,0.2320367,0.2841666,0.2524176,0.2185623,0.2666037,0.5372785,0.3462928,0.4944928),
#'                      discharge=c(0.004081598,0.004145695,0.005569669,0.005649690,0.024848787,0.025689611,0.059294500,0.020582500,0.009332000,0.021064750,0.016438000,0.010596000,0.017770950,0.139279063,0.040481681,0.092302000))
#'
#' Rating <- read.csv("C:/Users/jpayne05/OneDrive - Defra/Rating.csv")
#' rateOptimise(Discharge = rate$Discharge, Stage = rate$Stage)
#' rateOptimise(Discharge = rate$Discharge, Stage = rate$Stage, control = c(1.6, 2.0, 2.2))
#' z <- rateOptimise(Discharge = rate$Discharge, Stage = rate$Stage, control = 1.6)
rateOptimise <- function(Discharge, Stage, control = NULL, ...){
  dt <- data.table(Discharge, Stage)
  dl <- list()

  if(is.null(control)){
    dl[['Meta']] <- dt[, .(Limb = 1,
                           Range = 'Full',
                           minStage = min(Stage),
                           maxStage = max(Stage),
                           minDischarge = min(Discharge),
                           maxDischarge = max(Discharge))]
    dl[['nls']] <- minpack.lm::nlsLM(
      formula = Discharge ~ C*(Stage + a)^n,
      data = dt,
      start = list(C=1, a=0, n=1),
      control = nls.lm.control(maxiter = 100),
      ...
    )
  } else {
    dt$Limb <- cut(dt$Stage, breaks = c(-Inf, control, max(dt$Stage)), labels = 1:(length(control)+1), right = TRUE)
    dt$Range <- cut(dt$Stage, breaks = c(-Inf, control, max(dt$Stage)), right = TRUE)
    dl[['Meta']] <- dt[, .(
                           minStage = min(Stage),
                           maxStage = max(Stage),
                           minDischarge = min(Discharge),
                           maxDischarge = max(Discharge)), by = list(Limb, Range)]
    for(i in unique(dt$Limb)){
      dtl <-  dt[Limb == i,,]
      dl[[paste('NLS Limb', i)]] <- minpack.lm::nlsLM(
        formula = Discharge ~ C*(Stage + a)^n,
        data = dtl,
        start = list(C=1, a=0, n=1),
        control = nls.lm.control(maxiter = 100),
        ...
      )
    }
  }
  class(dl) <- append(class(dl), 'rating')
  return(dl)
}

#' Plot rating objects
#'
#' @param Discharge Flow series
#' @param Stage Level/stage series
#' @param fit Fittings derived from the rateOptimise function
#'
#' @return
#' @export
#'
#' @examples
#' z <- rateOptimise(Discharge = rate$Discharge, Stage = rate$Stage, control = 1.6)
#' plot.rating(Stage = rate$Stage, Discharge = rate$Discharge, fit = z)
plot.rating <- function(Stage, Discharge, fit){
  if('list' %in% class(fit)){
    plot(Discharge ~ Stage)
    for(i in seq_along(fit)[-1]){
      C <- coef(fit[[i]])['C']
      a <- coef(fit[[i]])['a']
      n <- coef(fit[[i]])['n']
      curve(C*(x+a)^n, add = TRUE, col = 2, lwd = 4, from = fit[])
    }
  }
}

plot.rating(Stage = rate$Stage, Discharge = rate$Discharge, fit = z)


z
as.numeric(as.character(z[['Meta']]$Range[1]))



# rate[, c('C', 'a', 'n') :={
#   power_nls <- minpack.lm::nlsLM(Discharge ~ C*(Stage + a)^n, data = .SD, start=list(C=1, a=0, n=1),)
#   params <-  list(C = coef(power_nls)["C"],
#                   a = coef(power_nls)["a"],
#                   n = coef(power_nls)["n"])
# },
# by = .(Group)]
#
#
# with(rate, plot(Stage, Discharge))
#
# for(i in unique(rate$Group)){
#   dt <-  rate[Group == i,,]
#   power_nls <- minpack.lm::nlsLM(Discharge ~ C*(Stage + a)^n, data = dt, start=list(C=1, a=0, n=1))
#   cat('Limb', i, 'Summary \n')
#   summary(power_nls)
#   C <- coef(power_nls)["C"]
#   a <- coef(power_nls)["a"]
#   n <- coef(power_nls)["n"]
#   # Plotting data and fitted curve
#   curve(C*(x+a)^n, add = TRUE, col = 2, lwd = 2)
# }
