#' @title Intersect of polygons
#'
#' @description Creates intersected polygons of catchment against voronoi polygons
#'
#' @param voronoi Voronoi/Thiessen polygon
#' @param catchment Catchment shapefile
#' @param coords Coordinates of rain gauges set as a polygon shapefile
#'
#' @return
#' @export
#'
#' @examples
#' #intersectPoly(voronoi = teesun(coords), catchment = shapefile, coords = coordinates)
intersectPoly <- function(voronoi, catchment, coords){
  cast <- st_cast(voronoi)
  intersect <- st_intersection(cast, shape_t)
  intersect_sf <- st_sf(intersect)
  join <- st_join(intersect_sf, coords, join = st_nearest_feature)
  return(join)
}

#' @title teesunPlot
#'
#' @description Creates a map of a catchment with Thiessen / Vornoi polyggons overlayed
#'
#' @param coords Coordinates of rain gauges set as a polygon shapefile
#' @param catchment Catchment shapefile
#'
#' @return
#' @export
#'
#' @examples
#' # teesunPlot(coords = coordinates, catchment = catchment)
#'
#' # z <- teesunPlot(coords = coords, catchment = shape_t)
#'
#' #z %>% addProviderTiles(providers$Stamen.Toner)
#' #z %>% addProviderTiles(providers$CartoDB.Positron)
#' #z %>% addProviderTiles(providers$Esri.NatGeoWorldMap)
teesunPlot <- function(coords, catchment){
  voronoi <- teeSun(coords, catchment = catchment)
  v_poly <- intersectPoly(voronoi = voronoi,
                      catchment = catchment,
                      coords = coords)
  names(v_poly)[1] <- 'ID'
  colors <- colorFactor(palette = c("green", "yellow", "red"),
                        domain = (v_poly$ID))
  p <- leaflet(v_poly) %>%
    addTiles() %>%
    addPolygons(data = voronoi,
                color = "blue",
                fillOpacity = 0.1,
                weight = 1.5) %>%
    addPolygons(fillColor = colors(v_poly$ID),
                fillOpacity = 0.7,
                weight = 1,
                popup = paste("<strong> Rain Gauge: </strong>",
                              v_poly$ID,
                              ' ',
                              "<strong> Area: </strong>",
                              round(st_area(v_poly)/1000000,2),
                              'km^2 ',
                              "<strong> Coverage: </strong>",
                              round(st_area(v_poly)/sum(st_area(v_poly))*100, 2),
                              '%')) %>%
    addCircleMarkers(data = coords,
                     color = "orangered",
                     radius = 6,
                     fillOpacity = 0.8,
                     stroke = FALSE) %>%
    addMarkers(data = coords,
               popup = ~htmlEscape(paste(ID)))
  return(p)
}
