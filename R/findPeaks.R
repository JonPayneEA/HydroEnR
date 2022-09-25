# link <- ("O:/National Modelling and Forecasting/21_Strategic Delivery/Flood Forecast Modelling/03 Training/Peak Detection/Allington.csv")
# allington <- read.csv(link, header = T)

# Function to find peak within a user defined window (m)
# Run this code to embed the function
#' @title Find peaks with base R
#'
#' @param x Data set
#' @param m Window of which to identify peaks
#'
#' @return
#' @export
#'
#' @examples
#' findPeaks(allington$Value, m = 5000)
#
# # This returns the row of where each peak is found
#
# peaks <- findPeaks(allington$Value, m = 5000)
# allington[peaks,] # Returns the rows where the peaks are found
#
# # Basic plot
# plot(allington$Value, type = 'l', main = paste0('m = 5000'))
# points(peaks, allington$Value[peaks], col = 'red', pch = 19)



# Loop through multiple m values and plot
# par(mfrow = c(2, 2))
# for(k in c(1000, 2000, 6000, 8000)){
#   p <- find_peaks(allington$Value, m = k)
#   ind <- rep(1, length(allington$Value))
#   ind[p] <- 2
#   plot(allington$Value, type = 'l', main = paste0('m = ', k))
#   #abline(h = 0.65)
#   points(p, allington$Value[p], col = 'red', pch = 19)
#   print(p)
# }
#
# z <- find_peaks(allington$Value, m = 1000)
findPeaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}
## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL
