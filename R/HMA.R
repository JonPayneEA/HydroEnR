#' John Ewens Hydrograph Matching Algorithm
#'
#' @description Created from the John Ewen 2011 paper; "Hydrograph matching method for measuring model performance".
#'
#' Three visual parameters (b, maxLag and maxLead) give the user flexibility when designing suitable performance measures.
#'
#' @param obs Observed series
#' @param sim Simulated series
#' @param b Scaling factor for timing errors, larger values degenerate into the NSE
#' @param maxLag Also known as w1, determines the number of time indices that be looked at from the observed backwards
#' @param maxLead Also known as w2, determines the number of time indices that be looked at from the observed forwards
#' @param measure Determines the objective
#' @param calcRays Option to calculate the rays for plotting. Set as TRUE.
#'
#' @return
#' @export
#'
#' @examples
#' qo <- c(3, 3, 4, 20, 62, 85, 33, 20, 5)
#' qs <- c(5, 12, 65, 43, 34, 28, 13, 12, 22)
#'
#' a <- HMA(obs = qo, sim = qs, b = 4, maxLag = 1, maxLead = 3, measure = 'nse')
#' plot(qo, type = 'l', col = 'blue', lwd = 2,
#'      xlab = 'Timestep',
#'           ylab = 'Flow')
#'           lines(qs, col = 'orange', lwd = 2)
#'           for (i in seq_along(a$Rays)){
#'             segments(a$Rays[[i]]$obs[1], a$Rays[[i]]$obs[2], a$Rays[[i]]$sim[1], a$Rays[[i]]$sim[2], lty = 2)
#'             }
#'
HMA <- function(obs = NULL, sim = NULL, b = 4, maxLag = 3, maxLead = 3, measure = 'nse', calcRays = TRUE){

  if (!is.numeric(obs) || !is.numeric(sim)) {
    stop("Observed and Simulated data must be numeric vectors")
  }

  if (length(obs) != length(sim)) {
    stop("Length of Observed and Simulated must be the same")
  }
  # Benchmark performance measure
  perfBench <- NULL
  if (measure %in% c('nse', 'square')) {
    perfBench <- sum((obs - mean(obs))^2)
  } else if (measure %in% c('mae', 'abs')) {
    perfBench <- sum(abs(obs - mean(obs)))
  } else {
    stop("HMA measure must be one of 'nse' or 'mae'")
  }

  # Calculate all matching pairs
  aw <- array(NA, c(length(obs), length(sim)))

  # Find E(j, k) or actual 'work'
  for (k in seq_along(obs)){
    for (j in max(1, k - maxLead):(min(length(sim), k + maxLag))){
      if (measure %in% c("nse", "square")) {
        aw[j, k] <- (sim[j] - obs[k])^2 + b^2 * (j - k)^2
      } else if (measure %in% c("mae", "abs")) {
        aw[j, k] <- abs(sim[j] - obs[k]) + b * abs(j - k)
      }
    }
  }

  # Calculate cumulative 'work' across the columns ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set up blank double array of (n, n, 2) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cw <- array(NA, dim = c(dim(aw), 2))
  # Populate first column ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # E(j,1) are placed into (j, 1, 1) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cw[, 1, 1] <- aw[, 1]


  # Cumulative
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  suppressWarnings({
    for (k in 2:length(obs)) {
      for (j in max(1, k - maxLead):min(length(sim), k + maxLag)) {
        e <- aw[j, k]
        cw[j, k, 2] <- e + cw[j, k - 1, 1]
        if (j > 1) {
          m1 <- min(cw[j - 1, k - 1,], na.rm = TRUE)
          if (j > 2) {
            m2 <- min(cw[j - 2, k - 1,], na.rm = TRUE)
          } else {
            m2 <- Inf
          }
          m <- min(m1, m2)
          cw[j, k, 1] <- sum(e, m, na.rm = TRUE)
        }
      }
    }
    # Coerce Inf to NA
    cw[is.infinite(cw)] <- NA
  })
  HMA <- list()
  HMA[['BenchPerf']] <- aw
  HMA[['CumulativePerf']] <- cw
  HMA[['BenchScore']] <- perfBench
  HMA[['CumulScore']] <- min(cw[, length(obs),], na.rm = TRUE)
  HMA[['Metric']] <- 1 - HMA[['CumulScore']]/HMA[['BenchScore']]

  if (calcRays == TRUE){
    res <- rep(NA, length(obs))
    tau <- rep(NA, length(obs))
    rays <- vector("list", length(obs))

    for (j in rev(seq_along(obs))) {
      if (j > 1){
        m <- which.min(cw[, j, 1])
        m_rep <- ifelse(is.na(which.min(cw[, j, 2])),
                        m,
                        which.min(cw[, j, 2]) )
        if (cw[m_rep, j, 2] <= cw[m, j, 1])
          m <- m_rep
      } else {
        m <- which.min(cw[, j, 1])
        m_rep <- m
      }
      # Export to vectors and lists
      tau[j] <- j - m
      res[j] <- obs[j] - sim[m]
      rays[[j]] <- list(obs = c(j, obs[j]), sim = c(m, sim[m]))

    }

    HMA[['Tau']] <- tau
    HMA[['Res']] <- res
    HMA[['Rays']] <- rays
  }
  return(HMA)
}
