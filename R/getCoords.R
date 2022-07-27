library(sf)
library(s2)

# Extract GGIS data from Metadata

getCoords <- function(...){
  lst <- list(...)
  coordsLst <- list()
  for(i in seq_along(lst)){
    name <- paste(lst[[i]]$Metadata[2,2])
    lat <- as.numeric(lst[[i]]$Metadata[15,2])
    long <- as.numeric(lst[[i]]$Metadata[14,2])

    coordsLst[[i]] <- data.table(ID = name, Lat = lat, Long = long)
  }
  dt <- rbindlist(coordsLst)
  projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  sf <- st_as_sf(x = dt,
                 coords = c("Long", "Lat"),
                 crs = st_crs(4326))
  return(sf)
}

# s <- getCoords(oundle, yelden, corby)
# s

# Make voronoi polygonn


teeSun <- function(x){
  if('sf' %in% class(x)){
    voronoi <- st_union(x)
    voronoi <- suppressWarnings(st_voronoi(voronoi))
    voronoi <- st_collection_extract(voronoi)
    return(voronoi)
  }
  stop('This function only works with data of class type "sf"')
}

# z <- teeSun(s)
#
# voronoi <- st_union(s)
# voronoi <- st_voronoi(s)
# voronoi <- st_collection_extract(voronoi)
#
# class(voronoi)
# class(s)
