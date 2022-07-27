library(sf)
library(leaflet)
library(htmltools)

# Voronoi in leaflet https://stackoverflow.com/questions/55048518/creating-bordering-polygons-from-spatial-point-data-for-plotting-in-leaflet

#Import shapefile
# shape <- st_read("C:/Users/jpayne05/Desktop/Chanters_Brook/FEH_Catchment_368450_404350.shp")
# shape_t <- st_transform(shape, "+init=epsg:4326", coords) # Change projection to WGS 84
#
# df_example <- read.csv(textConnection(
#   "Name,Lat,Long, Percent
#   Little Dimbleby,-2.482667, 53.53462, 15
#   De Pfeffle,-2.484541, 53.55108, 20
#   Suella Deville,-2.477480, 53.55954, 30
#   Mogster,-2.493072, 53.54557, 20
#   Raabish,-2.464458, 53.55841, 15"
# ))
#
# # Rain gauge points
# p1 <- st_point(c(-2.482667, 53.53462))
# p2 <- st_point(c(-2.484541, 53.55108))
# p3 <- st_point(c(-2.477480, 53.55954))
# p4 <- st_point(c(-2.493072, 53.54557))
# p5 <- st_point(c(-2.464458, 53.55841))
#
# d <- data.frame(a = df_example$Name, geometry = st_sfc(p1, p2, p3, p4, p5, crs = st_crs(4326)))
# # d$geometry <- st_sfc(p1, p2, p3, p4, p5, crs = st_crs(4326))
# df <- st_as_sf(d)
#
# #create the voronoi diagram, "some_variable" gets lost.
# rainVoronoi <- df %>%
#   st_union() %>%
#   st_voronoi() %>%
#   st_collection_extract()
#
# #do a spatial join with the original bw_sf data frame to get the data back
# v_poly <- st_cast(rainVoronoi) %>%
#   st_intersection(shape_t) %>%
#   st_sf() %>%
#   st_join(df, join = st_nearest_feature)
#
# #create a palette (many ways to do this step)
# colors <- colorFactor(palette = c("green", "yellow", "red"),
#   domain = (v_poly$a))
#
# #create the leaflet
# leaflet(v_poly) %>%
#   addTiles() %>%
#   addProviderTiles(providers$Stamen.Toner) %>%
#   # addProviderTiles(providers$CartoDB.Positron) %>%
#   # addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
#   addPolygons(data = rainVoronoi,
#               color = "blue",
#               fillOpacity = 0.1,
#               weight = 1.5) %>%
#   addPolygons(fillColor = colors(v_poly$a),
#               fillOpacity = 0.7,
#               weight = 1,
#               popup = paste("<strong> Rain Gauge: </strong>",
#                             v_poly$a,
#                             ' ',
#                             "<strong> Area: </strong>",
#                             round(st_area(v_poly)/1000000,2),
#                             'km^2 ',
#                             "<strong> Coverage: </strong>",
#                             round(st_area(v_poly)/sum(st_area(v_poly))*100, 2),
#                             '%')) %>%
#   addCircleMarkers(data = df,
#                    color = "orangered",
#                    radius = 6,
#                    fillOpacity = 0.8,
#                    stroke = FALSE) %>%
#   addMarkers(data = df,
#              popup = ~htmlEscape(paste(a))) %>%
#   setView(-2.482667, 53.55954 , zoom = 13)
