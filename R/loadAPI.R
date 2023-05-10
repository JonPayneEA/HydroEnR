library(ggplot2)
#' @title Pull data from the Environment Agency's API
#'
#' @description The loadAPI function is used to interogate the EAs API. Data for
#'  all station can be pulled, or when specified to a single site various
#'  datasets can be pulled.
#'
#' @param ID Use to specify a particular WISKI ID or for when downloading all
#' stations ' wiski' or 'nrfa' to filter results
#' @param measure Use this when exporting observations to select the available
#' parameter. Generally 'flow', 'level', or 'groundwater'
#' @param period  This is so you can select the time steps available, generally
#' 900 (15 min) or 86400 (daily)
#' @param type  Selects the data type, instantaneous, min, max etc.
#' @param datapoints Allows you to decide where in the time series you are
#' pulling the data from
#' @param from First time step of choice
#' @param to Last time step of choice
#' @param lat Latitude coordinate for when searching by geographic area
#' @param long Longitude coordinate for when searching by geographic area
#' @param easting Easting coordinate for when searching by geographic area
#' @param northing Northing coordinate for when searching by geographic area
#' @param dist Distance in km for how far you wish to search
#' @param obsProperty Used to filter the stations when identifying sites.
#' Available metrics;
#' 'waterFlow', 'waterLevel', 'rainfall', 'groundwaterLevel', 'ammonium',
#' 'dissolved-oxygen', 'conductivity', 'ph', 'temperature', 'turbidity',
#' 'nitrate', 'chlorophyll', 'salinity', 'bga', 'fdom'
#'
#' @import jsonlite
#' @import tidyr
#' @import ggplot2
#'
#' @return
#' @export
#'
#' @examples
#' loadAPI()
#'
#' loadAPI(easting = 378235 , northing = 276165, dist = 30, ID = 'nrfa')
#'
#' loadAPI(ID = 'L1207')
#'
#' loadAPI(ID = 'L1207',
#' measure = 'level',
#' period = 900,
#' type = 'instantaneous',
#' datapoints = 'earliest')
#'
#' dt <- loadAPI(ID = 'L1207',
#' measure = 'level',
#' period = 900,
#' type = 'instantaneous',
#' datapoints = 'all')
#'
#' with(dt, plot(value ~ dateTime,
#'type = 'l',
#'xlab = 'Time',
#'ylab = 'Stage (mAoD)'))
loadAPI <- function(ID = NULL, measure = NULL, period = NULL,
                    type = NULL, datapoints = 'standard',
                    from = NULL, to = NULL, lat = NULL, long = NULL,
                    easting = NULL, northing = NULL, dist = NULL,
                    obsProperty = NULL){
  #Initial start up check ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  obsVars <- c('waterFlow', 'waterLevel', 'rainfall', 'groundwaterLevel',
               'ammonium', 'dissolved-oxygen', 'conductivity', 'ph',
               'temperature', 'turbidity', 'nitrate', 'chlorophyll', 'salinity',
               'bga', 'fdom')
  if (!is.null(obsProperty) &&!obsProperty %in% obsVars){
    return(warning('Observed property does not match those available'))
  }

  # Start up of main function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dataType <- type # Fixes data.table filter issue
  timestep <- period # Fixes data.table filter issue
  # Base link for the EAs API
  baselink <- 'http://environment.data.gov.uk/hydrology/id/stations.json'

  # Return all available stations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if ((is.null(ID)|| ID %in% c('all', 'wiski', 'nrfa')) &
      is.null(easting) & is.null(lat) &  is.null(long) & is.null(northing) &
      is.null(dist)){
    ## Limit set to 20,000 (current API is ~8,000) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    datalink <- paste0(baselink, '?_limit=20000')
    dt <- data.table(jsonlite::fromJSON(datalink)$items)
    ## Unnesting observed property is slightly more difficult ~~~~~~~~~~~~~~~~~~
    ## Stored as dataframe list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Remove URL and convert dataframes to string ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dt_obs <- dt$observedProperty
    lst <- list()
    for (i in seq_along(dt_obs)){
      lst[[i]] <- basename(dt_obs[[i]]$`@id`)
    }
    dt$observedProperty <- lst
    ## Check if any columns are class list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if ('list' %in% sapply(dt, class)){
      dt <- data.table(tidyr::unnest(dt, c(wiskiID, label,
                                           riverName, easting, northing, lat,
                                           long, observedProperty),
                                     keep_empty = TRUE))
    }

    ## Select relevant columns ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    data_level <- dt[, .(wiskiID, label, riverName,
                         observedProperty, easting, northing, lat,
                         long, dateOpened, catchmentArea, nrfaStationID),]
    # data_level$observedProperty <- unlist(lst)
    if (!is.null(ID) && ID == 'wiski')
      data_level <- na.omit(data_level, cols = 'wiskiID')
    if (!is.null(ID) && ID == 'nrfa')
      data_level <- na.omit(data_level, cols = 'nrfaStationID')
    if(is.null(obsProperty)){
      return(data_level)
    } else {
      return(data_level[observedProperty == obsProperty,,])
    }
  }

  # Find available stations within a set distance ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if ((is.null(ID)|| ID %in% c('all', 'wiski', 'nrfa')) &
      (!is.null(easting) | !is.null(lat)) &
      (!is.null(northing) | !is.null(long)) &
      !is.null(dist)){
    ## Checking for duplicate coordinates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if((!is.null(easting)|!is.null(northing)) &
       (!is.null(lat) & !is.null(long)))
      return(warning('Conflicting coordinate systems, please use  lat-long OR
                     easting-northing'))
    ## Create different links depending of coordinates used ~~~~~~~~~~~~~~~~~~~~
    if (!is.null(easting) & !is.null(northing)) {
      datalink <- paste0(baselink, '?easting=', easting,
                         '&northing=', northing, '&dist=', dist)
    } else if  (!is.null(lat) & !is.null(long)){
      datalink <- paste0(baselink, '?lat=', lat, '&long=',long, '&dist=', dist)
    } else {
      return(warning('Please check your coordinate data'))
    }
    dt <- data.table(jsonlite::fromJSON(datalink)$items)
    ## Unnesting observed property is slightly more difficult ~~~~~~~~~~~~~~~~~~
    ## Stored as dataframe list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Remove URL and convert dataframes to string ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dt_obs <- dt$observedProperty
    lst <- list()
    for (i in seq_along(dt_obs)){
      lst[[i]] <- basename(dt_obs[[i]]$`@id`)
    }
    dt$observedProperty <- lst
    ## Check if any columns are class list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if ('list' %in% sapply(dt, class)){
      dt <- data.table(tidyr::unnest(dt, c(wiskiID, label,
                                           riverName, easting, northing, lat,
                                           long, observedProperty),
                                     keep_empty = TRUE))
    }
    ## Select relevant columns ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Catchment area is sometimes a missing field ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Replicated as NAs for consistency ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!'catchmentArea' %in% colnames(dt))
      dt$catchmentArea <- rep('NA', times = dim(dt)[1])
    data_level <- dt[, .(wiskiID, label, riverName,
                         observedProperty, easting, northing, lat,
                         long, dateOpened, catchmentArea, nrfaStationID),]
    ## Additional is.null(ID) level, resolves problems with needless arguments ~
    if (is.null(ID)){
      return(data_level)
    }
    if (!is.null(ID) && ID == 'wiski')
      data_level <- na.omit(data_level, cols = 'wiskiID')
    if (!is.null(ID) && ID == 'nrfa')
      data_level <- na.omit(data_level, cols = 'nrfaStationID')
    if(is.null(obsProperty)){
      return(data_level)
    } else {
      return(data_level[observedProperty == obsProperty,,])
    }
  }

  # Return options for available data at specified ID site ~~~~~~~~~~~~~~~~~~
  if (!is.null(ID)&is.null(measure)&is.null(timestep)){
    link <- paste0(baselink, '?wiskiID=', ID)
    data <- jsonlite::fromJSON(link)
    data_level <- jsonlite::fromJSON(as.character(data$items[1]))
    params <- data.table(parameter = data_level$items$measures[[1]]$parameter,
                         period = data_level$items$measures[[1]]$period,
                         type = data_level$items$measures[[1]]$valueType)
    return(params)
  }

  # Return data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(ID)&!is.null(measure)&!is.null(period)){
    link <- paste0(baselink, '?wiskiID=', ID)
    data <- jsonlite::fromJSON(link)
    data_level <- jsonlite::fromJSON(as.character(data$items[1]))
    params <- data.table(parameter = data_level$items$measures[[1]]$parameter,
                         period = data_level$items$measures[[1]]$period,
                         type = data_level$items$measures[[1]]$valueType,
                         note = data_level$items$measures[[1]]$notation)
    ## Insert criteria on what data to extract ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Merges into URL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    datalink <- params[parameter == measure &
                         period == timestep &
                         type == dataType,
                       note,]
    measImp <- paste0('http://environment.data.gov.uk/hydrology/id/measures/',
                      datalink)
    ## Additional URL commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (datapoints == 'all')
      datalinkAppend <- paste0(measImp, '/readings.json?_limit=2000000')
    if (datapoints == 'earliest')
      datalinkAppend <- paste0(measImp, '/readings.json?earliest')
    if (datapoints == 'latest')
      datalinkAppend <- paste0(measImp, '/readings.json?latest')
    if (datapoints == 'standard')
      datalinkAppend <- paste0(measImp, '/readings.json')
    if (datapoints == 'day'){
      minDate <- as.Date(from)
      datalinkAppend <- paste0(measImp, '/readings.json?date=', minDate)
    }
    if (datapoints == 'range'){
      minDate <- as.Date(from)
      maxDate <- as.Date(to)
      datalinkAppend <- paste0(measImp, '/readings.json?mineq-date=',
                               minDate, '&max-date=',maxDate)
    }
    series <- data.table(jsonlite::fromJSON(datalinkAppend)[[2]])
    ## For clarity all dates are coerced to POSIXct ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Currently split between 2 timesteps to future proof ~~~~~~~~~~~~~~~~~~~~~
    if (timestep == 900){
      series$dateTime <- as.POSIXct(series$dateTime,
                                    format = '%Y-%m-%dT%H:%M',
                                    tz = 'GMT')
    }
    if (timestep == 86400){
      series$dateTime <- as.POSIXct(series$dateTime,
                                    format = '%Y-%m-%dT%H:%M',
                                    tz = 'GMT')
    }
    return(series[,-1:-2])
  }
}





