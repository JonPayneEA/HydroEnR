#' @title Extreme Volume Plots
#'
#' @param x HydroVAMAX Dataset
#' @param duration At the moment this function can only process one duration at a time. Select the column name
#' @param scaling Scale to QMED, set as TRUE
#' @param filter Set to null, which selects all. Use vector of distributions to select models; 'GEV', 'Gumbel', 'GenLog', and 'GenPareto'
#' @param secX Secondary X axis to highlight return periods
#' @param ppy Peaks per year used in the growth function equations
#' @param URBEXT2000 Used in the growth function equations
#' @param DeUrb Used in the growth function equations
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' amax <- getAMAX(rnrfa::get_ts(id = 2001, type = 'amax-flow'))
#' plotExtVal(amax)
#' plotExtVal(amax, scaling = FALSE, filter = c('GEV', 'GenLog'), URBEXT2000 = 0.4)
plotExtVol <- function(x, duration = NULL, scaling = TRUE, filter = NULL, secX = TRUE, ppy = 1, URBEXT2000 = NULL, DeUrb = FALSE){
  if('HydroVAMAX' %in% class(x)==FALSE){
  stop('Data not of class "HydroVAMAX" ')
  }
  if(is.null(duration)){
    print(colnames(x))
    stop('No duration selected, please select from the above')
  }
  x <- as.vector(as.matrix(x[,..duration,]))
  len <- length(x)
  qmed <- QMED(x) # Though this is VMED the QMED equation will work
  rankSeq <- seq(1000, 1)
  gringRank <- (rankSeq - 0.44) / (1000 + 0.12)
  logRedVar <- log((1/gringRank) - 1)

  obsRank <- seq(len, 1)
  gringObs <- (obsRank - 0.44) / (len + 0.12)
  logRedVarObs <- log((1/gringObs) - 1)

  if (scaling == TRUE) {
    scale <- x/qmed
    incAMAX <- sort(scale, decreasing = FALSE)
    ss <- growthFactors(x, RP = 1/gringRank, ppy = ppy, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
    ylab <- 'V / VMED'
    intercept <- 1
  } else {
    incAMAX <- sort(x, decreasing = FALSE)
    ss <- growthFactors(x, RP = 1/gringRank, ppy = ppy, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
    cols <- c('GEV', 'Gumbel', 'GenLog', 'GenPareto' )
    ss <- ss[ , (cols) := lapply(.SD, '*', qmed), .SDcols = cols]
    ylab <- expression(Total~Flow ~ m^3)
    intercept <- qmed
  }
  point <- data.table(logRedVarObs, incAMAX)
  dt <- data.table(logRedVar, ss)
  dtm <- melt.data.table(dt, id.vars = 1:2)
  setnames(dtm, old = 'variable', new = 'Distribution')
  if(!is.null(filter)){
    filts_match <- paste0(filter, collapse = '|')
    dtm <- dtm[Distribution %like% filts_match,,]
  }
  secAxis <- c(2, 5, 10, 20, 50, 100, 500, 1000)
  secAxisL <- log(secAxis - 1)


  p <- ggplot(dtm, aes(x = logRedVar, y = value, colour = Distribution))+
    geom_hline(yintercept = intercept, linetype = 'dashed', size = 1.5, colour = 'darkgrey') +
    geom_vline(xintercept = 0, linetype = 'dashed', size = 1.5, colour = 'darkgrey') +
    geom_line(size = 2) +
    geom_point(inherit.aes = FALSE,
               data = point,
               aes(x = logRedVarObs, y = incAMAX, size = 1),
               shape = 21, colour = 'black', fill = 'lightblue', stroke = 1.5) +
    ylab(ylab) +
    xlab('Logged Reduced Variable') +
    ggtitle('Extreme Volume Plot') +
    guides(size = guide_legend(title = 'Observed', label = FALSE))

  if(secX == TRUE){
    miny <- min(dtm$value)
    maxy <- max(dtm$value)
    pos <- (intercept + miny)/2.5
    p <- p + geom_segment(aes(x = min(secAxisL),
                              y = pos,
                              xend = max(secAxisL),
                              yend = pos),
                          size = 1, colour = 'black')
    for (i in seq_along(secAxisL)){
      p <- p + geom_segment(x = secAxisL[i], y = pos,
                            xend = secAxisL[i], yend = pos + (pos*0.05),
                            size = 1, colour = 'black')
      p <- p + geom_text(x = secAxisL[i], y = pos + (pos*0.15),
                         label = secAxis[i], colour = 'black')
    }
  }
  return(p)
}

