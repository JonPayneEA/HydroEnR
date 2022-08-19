#' @title Merge Rain gauge data
#'
#' @description Data from rain gauges are merged via this function using a weighted average
#'
#' @param gaugeProp Data table calculated with the gaugeProp() function
#' @param rainAll Data table of collated rain gauge data created with the mergeData() function
#'
#' @return
#' @export
#'
#' @examples
#' #rainAvg(prop, rain)
rainAvg <- function(gaugeProp, rainAll){
  input<-NULL
  x<-NULL
  input <- menu(c("Yes", "No"),
                title = "Do the order of gauges in the gauge proportions and merged rain gauge data match")
  if (input == 1){
    x<-NULL
    message('Proceding to rainfall averaging')
    if(dim(prop)[1] - (dim(rain)[2]-1) != 0){
      stop('Number of sites do not match')
    }
    weights <- rainAll[,-1,]
    weights <- weights[, lapply(.SD, function(x) ifelse(is.na(x), 0, 1)), ]
    for (col in 1:ncol(weights)) {
      set(weights, j = col, value = weights[[col]] * (gaugeProp$Proportion[col]/100))
    }
    weights[, areaSum := rowSums(.SD, na.rm = TRUE),]
    product <- rainAll[,-1,] * weights[, 1:(length(weights)-1),]
    product[, Merged := rowSums(.SD, na.rm = TRUE)/weights$areaSum,]
  dt <- data.table(rainAll, Merged = product$Merged)
    class(dt) <- append(class(dt), 'rainAll')
    return(dt)
  }
  if (input == 2){
    stop('Site order must match, please reorganise the list using mergeData()')
  }
}
