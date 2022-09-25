#' @title Add convert flow or rainfall data to a volume
#'
#' @param x Data set of 'flowLoad' or 'rainLoad' class
#' @param area_km Area of coverage for rain gauges
#'
#' @return
#' @export
#'
#' @examples
#' # oundle <- loadWISKI(files[4])
#' # oundle
#' # asVol.rainLoad(oundle, area = 40)
#' # oundle
#' #
#
#' # link <- 'C:/Users/jpayne05/Desktop/Buildwas_15min_Flow.csv'
#' # Buildwas <- loadWISKI(link)
#' # Buildwas
#' # asVol.flowLoad(Buildwas)
#' # Buildwas
asVol <- function(x, area_km){
  UseMethod('asVol', x)
}

#' @rdname asVol
#' @export
asVol.flowLoad <- function(x, area_km = NULL){
  tDiff <- difftime(x$GaugeData$DateTime[2],
                    x$GaugeData$DateTime[1],
                    units = 'mins')
  tDiff <- as.numeric(tDiff)
  x$GaugeData[, Volume := Value * tDiff * 60]
  return(x)
}

#' @rdname asVol
#' @export
asVol.rainLoad <- function(x, area_km = NULL){
  if(is.null(area_km)){
    stop('Please specify the catchment area')
  }
  area <- area_km * 1000000
  tDiff <- difftime(x$GaugeData$DateTime[2],
                    x$GaugeData$DateTime[1],
                    units = 'mins')
  tDiff <- as.numeric(tDiff)
  x$GaugeData[, Volume := (Value/1000) * area]
  return(x)
}
