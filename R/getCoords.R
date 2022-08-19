#' @title getCoords
#'
#' @description This extracts the site name, Latitude, and Longitude data from the imported
#' WISKI metadata associated with each file. Outputs are a shapefile of class SF.
#'
#' @param ... The files you wish to include
#'
#' @return
#' @export
#'
#' @import sf
#' @import s2
#'
#' @examples
#' getCoords(oundle, yelden, corby)
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

#' @title teeSun
#'
#' @description Creates a Thiessen/Voronoi polygon shapefile.
#'
#' @param x SF file with point data within.
#' @param catchment Set as null, optional bounding box polygon derived from catchment polygon
#'
#' @return
#' @export
#'
#' @examples
#' teeSun(s)
teeSun <- function(x, catchment = NULL){
  if('sf' %in% class(x)){
    if(is.null(catchment)){
      voronoi <- st_union(x)
      voronoi <- suppressWarnings(st_voronoi(voronoi))
      voronoi <- st_collection_extract(voronoi)
    } else {
      bbox <- st_bbox(catchment) + c(-0.5, -0.2, 0.5, 0.2)
      bbox <-  st_as_sfc(bbox)
      voronoi <- st_union(x)
      voronoi <- suppressWarnings(st_voronoi(voronoi, envelope = bbox))
      voronoi <- st_collection_extract(voronoi)
    }
    return(voronoi)
  }
  stop('This function only works with data of class type "sf"')
}

