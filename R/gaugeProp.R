#' @title Gauge proportion
#'
#' @description Calculates the the proportion of catchment that the gauge occupies using Thiessen / Voronoi polygons
#'
#' @param coords Coordinates of rain gauges set as a polygon shapefile
#' @param catchment Catchment shapefile
#'
#' @return
#' @export
#'
#' @examples
#' #gaugeProp(coords, shape_t)
gaugeProp <- function(coords, catchment){
  voronoi <- teeSun(coords, catchment)
  v_poly <- intersectPoly(voronoi = voronoi,
                      catchment = catchment,
                      coords = coords)
  names(v_poly)[1] <- 'ID'
  area <- round(as.numeric(st_area(v_poly)/1000000,2)) # calculates in km^2
  total <- sum(area)
  prop <- (area/total) * 100 # Percentage area
  dt <- data.table(Gauge = v_poly$ID, Area = area, Proportion = prop)
  class(dt) <- append(class(dt), 'gaugeProp')
  return(dt)
}

