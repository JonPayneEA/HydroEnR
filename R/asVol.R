
asVol.flowLoad <- function(x, area_km = NULL, time = NULL){
  tDiff <- difftime(x$GaugeData$DateTime[2],
                    x$GaugeData$DateTime[1],
                    units = 'mins')
  tDiff <- as.numeric(tDiff)
  x$GaugeData[, Volume := Value * tDiff * 60]
  return(x)
}

asVol.rainLoad <- function(x, area_km = NULL, time = NULL){
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




# oundle <- loadWISKI(files[4])
# oundle
# asVol.rainLoad(oundle, area = 40)
# oundle
#
#
# link <- 'C:/Users/jpayne05/Desktop/Buildwas_15min_Flow.csv'
# Buildwas <- loadWISKI(link)
# Buildwas
# asVol.flowLoad(Buildwas)
# Buildwas
