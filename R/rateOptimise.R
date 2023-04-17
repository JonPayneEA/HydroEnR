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
#' #library(data.table)
#' #library(minpack.lm)
#' #gaugings <- data.frame(Stage = c(1.855, 2.109, 2.037, 1.972, 2.574, 1.748, 2.016),
#'  #                      Discharge = c(177.685, 240.898, 221.954, 205.55, 383.051, 154.061, 216.582))
#'
#' # Writing data into an R dataframe:
#' #gaugings <- data.frame(stage=c(0.1374976,0.1354978,0.1547908,0.1547908,0.3047804,0.3022808,0.4138614,0.2901140,0.2320367,0.2841666,0.2524176,0.2185623,0.2666037,0.5372785,0.3462928,0.4944928),
#'  #                    discharge=c(0.004081598,0.004145695,0.005569669,0.005649690,0.024848787,0.025689611,0.059294500,0.020582500,0.009332000,0.021064750,0.016438000,0.010596000,0.017770950,0.139279063,0.040481681,0.092302000))
#'
#' #Rating <- read.csv("C:/Users/jpayne05/OneDrive - Defra/Rating.csv")
#' #rateOptimise(Discharge = rate$Discharge, Stage = rate$Stage)
#' #rateOptimise(Discharge = Rating$Discharge, Stage = Rating$Stage, control = c(1.6, 2.0, 2.2))
#' #z <- rateOptimise(Discharge = Rating$Discharge, Stage = Rating$Stage, control = 1.6)
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
    dt$Limb <- cut(dt$Stage, breaks = c(min(dt$Stage), control, max(dt$Stage)), labels = 1:(length(control)+1), right = TRUE, include.lowest = TRUE)
    dt$Range <- cut(dt$Stage, breaks = c(min(dt$Stage), control, max(dt$Stage)), right = TRUE, include.lowest = TRUE)
    dt$lowerRange <- read.table(text = gsub("[^.0-9]", " ", dt$Range), col.names = c("lowerRange", "upperRange"))[,1]
    dt$upperRange <- read.table(text = gsub("[^.0-9]", " ", dt$Range), col.names = c("lowerRange", "upperRange"))[,2]
    dl[['Meta']] <- dt[, .(
                           minStage = min(Stage),
                           maxStage = max(Stage),
                           minDischarge = min(Discharge),
                           maxDischarge = max(Discharge)), by = list(Limb, lowerRange, upperRange)]
    dl[['Data']] <- dt
    for(i in unique(dt$Limb)){
      dtl <-  dt[Limb == i,,]
      dl[[paste('NLS Limb', i)]] <- minpack.lm::nlsLM(
        formula = Discharge ~ C*(Stage + a)^n,
        data = dtl,
        start = list(C=1, a=0, n=1),
        control = minpack.lm::nls.lm.control(maxiter = 100),
        ...
      )
    }
  }
  class(dl) <- append(class(dl), 'OptRating')
  return(dl)
  # return(dt)
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
#' # z <- rateOptimise(Discharge = rate$Discharge, Stage = rate$Stage, control = 1.6)
#' # ratingPlot(fit = z, colours = c(5, 2, 3, 4))
ratingPlot <- function(fit, colours = NULL){
  if(length(colours) != dim(fit[['Meta']])[1]){
    colours <- NULL
    print('Number of colours listed does not match the limb quantity. Colours have been coerced to 1.')
  }
  with(fit[['Data']], plot(Discharge ~ Stage))
  for(i in seq_along(fit)[-1:-2]){
    C <- coef(fit[[i]])['C']
    a <- coef(fit[[i]])['a']
    n <- coef(fit[[i]])['n']
    j <- i - 2
    ifelse(is.null(colours),
           curve(C*(x+a)^n, add = TRUE, col = 2, lwd = 4, from = fit[['Meta']]$lowerRange[j], to = fit[['Meta']]$upperRange[j]),
           curve(C*(x+a)^n, add = TRUE, col = colours[j], lwd = 4, from = fit[['Meta']]$lowerRange[j], to = fit[['Meta']]$upperRange[j]))
  }
  abline(v = fit[['Meta']]$upperRange[1:dim(fit[['Meta']])[1]-1], col = 2, lwd = 2, lty = 2)
}
