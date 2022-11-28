# library(jsonlite)
# library(data.table)
# library(ggplot2)
#
# ##### Set up a data pulling function
# ### Converts dateTime into a functional format
# # run this code to include function in environment
# hydroAPI <- function(WISKI = NULL, Parameter = NULL, Period = NULL, Type = 'instantaneous', datapoints = 'standard', UAT = FALSE){
#   if(UAT == FALSE){
#     baselink <- 'http://environment.data.gov.uk/hydrology/id/stations.json'
#     if(!is.null(WISKI)&is.null(Parameter)&is.null(Period)){
#       link <- paste0(baselink, '?wiskiID=', WISKI)
#       data <- jsonlite::fromJSON(link)
#       data_level <- jsonlite::fromJSON(as.character(data$items[1]))
#       params <- data.table::data.table(Parameter = data_level$items$measures[[1]]$parameter,
#                                        Period = data_level$items$measures[[1]]$period,
#                                        Type = data_level$items$measures[[1]]$valueType)
#       # data$Type <- gsub(pattern = '(http[s]?://)?([^/s]+/)', x = data$Type , replacement = '')
#       return(params)
#     }
#     if(!is.null(WISKI)&!is.null(Parameter)&!is.null(Period)){
#       link <- paste0(baselink, '?wiskiID=', WISKI)
#       data <- jsonlite::fromJSON(link)
#       data_level <- jsonlite::fromJSON(as.character(data$items[1]))
#       params <- data.table::data.table(parameter = data_level$items$measures[[1]]$parameter,
#                                        period = data_level$items$measures[[1]]$period,
#                                        type = data_level$items$measures[[1]]$valueType,
#                                        note = data_level$items$measures[[1]]$notation)
#       datalink <- params[parameter == Parameter & period == Period & type == Type, note,]
#       measure <- paste0('http://environment.data.gov.uk/hydrology/id/measures/',
#                         datalink)
#       if(datapoints == 'all')
#         datalinkAppend <- paste0(measure, '/readings.json?_limit=2000000')
#       if(datapoints == 'earliest')
#         datalinkAppend <- paste0(measure, '/readings.json?earliest')
#       if(datapoints == 'standard')
#         datalinkAppend <- paste0(measure, '/readings.json')
#       series <- data.table(jsonlite::fromJSON(datalinkAppend)[[2]])
#       if(Period == 900){
#         series$dateTime <- as.POSIXct(series$dateTime, format = '%Y-%m-%dT%H:%M', tz = 'GMT')
#       }
#       if(Period == 86400){
#         series$dateTime <- as.POSIXct(series$dateTime, format = '%Y-%m-%dT%H:%M', tz = 'GMT')
#       }
#       return(series[,3:5])
#     }
#   }
#   if(UAT == TRUE){
#     baselink <- 'https://hydrology-uat.epimorphics.net/hydrology/id/stations.json'
#     if(!is.null(WISKI)&is.null(Parameter)&is.null(Period)){
#       link <- paste0(baselink, '?wiskiID=', WISKI)
#       data <- data.table(jsonlite::fromJSON(link)[[2]]$measures[[1]])
#       colnames(data) <- c('ID', 'Parameter', 'Period', 'Type')
#       data$Type <- gsub(pattern = '(http[s]?://)?([^/s]+/)', x = data$Type , replacement = '')
#       return(data[,2:4])
#     }
#     if(!is.null(WISKI)&!is.null(Parameter)&!is.null(Period)){
#       link <- paste0(baselink, '?wiskiID=', WISKI)
#       data <- data.table(jsonlite::fromJSON(link)[[2]]$measures[[1]])
#       colnames(data) <- c('ID', 'parameter', 'period', 'valueType')
#       data$valueType <- gsub(pattern = '(http[s]?://)?([^/s]+/)', x = data$valueType , replacement = '')
#       datalink <- data[parameter == Parameter & period == Period & valueType == Type, ID,]
#       datalinkupd <- summary(url(datalink))$description
#       cut <- gsub(pattern = '(http[s]?://)?([^/s]+/)', x = datalinkupd , replacement = '')
#       uat <- paste0('https://hydrology-uat.epimorphics.net/hydrology/id/',
#                     cut) # During UAT we have to swap to the UAT portal
#       # data_level <- jsonlite::fromJSON(as.character(datalink))
#       if(datapoints == 'all')
#         datalinkAppend <- paste0(uat, '/readings.json?_limit=2000000')
#       if(datapoints == 'earliest')
#         datalinkAppend <- paste0(uat, '/readings.json?earliest')
#       if(datapoints == 'standard')
#         datalinkAppend <- paste0(uat, '/readings.json')
#       series <- data.table(jsonlite::fromJSON(datalinkAppend)[[2]])
#       if(Period == 900){
#         series$dateTime <- as.POSIXct(series$dateTime, format = '%Y-%m-%dT%H:%M', tz = 'GMT')
#       }
#       if(Period == 86400){
#         series$dateTime <- as.POSIXct(series$dateTime, format = '%Y-%m-%dT%H:%M', tz = 'GMT')
#       }
#       return(series[,3:6])
#     }
#   }
# }
#
# ## datapoints
# # standard uses soft limit of 100,000 rows
# # all coerces to the maximum of 2,000,000 rows
# # earliest returns the first datapoint
# # more to come
#
# ## As UAT is not live the function has 2 versions
# # UAT as TRUE pulls via the UAT portal
# # UAT FALSE pulls from the EA API
#
# # Check available parameters
# hydroAPI(WISKI = 'SX87U053')
# hydroAPI(WISKI = 'SX87U053', UAT = TRUE) # Note that though lots of different data are displayed they aren't all live yet
#
# hydroAPI(WISKI = '188753', UAT = FALSE)
# hydroAPI(WISKI = '188753', UAT = TRUE)
#
# # Pull data
# hydroAPI(WISKI = 'SX87U053',
#          Parameter = 'flow',
#          Period = 900,
#          Type = 'instantaneous',
#          datapoints = 'standard')
#
# hydroAPI(WISKI = 'SX87U053',
#          Parameter = 'level',
#          Period = 900,
#          Type = 'instantaneous',
#          datapoints = 'standard',
#          UAT = TRUE)
#
# hydroAPI(WISKI = '188753',
#          Parameter = 'rainfall',
#          Period = 86400,
#          Type = 'total',
#          datapoints = 'standard',
#          UAT = TRUE)
#
# ########## Download a full set of data example
# # Sutton Gault (Gt. Ouse)stage
#
# hydroAPI(WISKI = 'D33811') # No data on current API
# hydroAPI(WISKI = 'D33811', UAT = TRUE) # Present in UAT
#
# SGault <- hydroAPI(WISKI = 'D33811',
#                    Parameter = 'level',
#                    Period = 900,
#                    datapoints = 'all',
#                    UAT = TRUE)
#
# plot(SGault$value ~ SGault$dateTime,
#      col = as.numeric(as.factor(unique(SGault$quality))),
#      type = 'l',
#      ylim = c(0,5),
#      main = 'Sutton Gault stage (m)',
#      xlab = 'Time',
#      ylab = 'Stage (m)')
#
# # More advanced plots
# ggplot(SGault, aes(x = dateTime, y = value, colour = quality)) +
#   geom_path(aes(group = 1)) +
#   ggtitle('Sutton Gault stage (m)') +
#   xlab('Time') +
#   ylab('Stage (m)') +
#   ylim(0, 4.5)
#
#
# # Filter for only 'Good' data
#
# SGault_good <- SGault[quality == 'Good',,]
# ggplot(SGault_good, aes(x = dateTime, y = value)) +
#   geom_path() +
#   ggtitle('Sutton Gault stage (m)') +
#   xlab('Time') +
#   ylab('Stage (m)') +
#   ylim(0, 4.5)
#
#
# ######### Download all stations meta
# ### Can take a while but useful data
# link <- 'https://hydrology-uat.epimorphics.net/hydrology/id/stations.json?_limit=2000000'
# stations <- jsonlite::fromJSON(txt = link, flatten = TRUE)[[2]]
# names(stations)[names(stations) == '@id'] <- 'Ref'
# # Select first value from list
# stations$label <- sapply(stations$label,"[[",1)
# # On testingg 2nd WISKI ID relates to 404 on API - removing from list
# stations$wiskiID <- sapply(stations$wiskiID,"[[",1)
# # More complex list structures - coerce to square vector
# stations$riverName <- sapply(stations$riverName,"[[",1)
# stations$riverName <- sapply(stations$riverName, paste, collapse = ' ')
# stations$colocatedStation <- sapply(stations$colocatedStation,"[[",1)
# stations$colocatedStation <- sapply(stations$colocatedStation, paste, collapse = ' ')
# stations$observedProperty <- sapply(stations$observedProperty,"[[",1)
# names(stations$observedProperty) <- NULL
# stations$observedProperty <- sapply(output,"[[",1) # rerun to correctly format
#
# # Decide on values grepl wants to class as surface water
# toMatch <- c('waterLevel', 'waterFlow')
# for(i in seq_along(stations$observedProperty)){
#   if(stations$observedProperty[[i]] == 'http://environment.data.gov.uk/reference/def/op/rainfall')
#     stations$observedProperty[[i]] <- 'rainfall'
#   if(stations$observedProperty[[i]] == 'http://environment.data.gov.uk/reference/def/op/groundwaterLevel')
#     stations$observedProperty[[i]] <- 'groundwaterLevel'
#   if(length(grep(paste(toMatch,collapse = '|'), stations$observedProperty[[i]])) > 0)
#     stations$observedProperty[[i]] <- 'surfaceWater'
# }
#
# measures <- stations$measures
# for(i in seq_along(measures)){
#   if(class(measures[[i]])=='data.frame'){
#     measures[[i]]$Ref <- rep(stations$`@id`[i], dim(measures[[i]])[1])
#     measures[[i]] <- data.table::data.table(measures[[i]])
#   }
#   if(class(measures[[i]]) =='list'){
#     measures[[i]]$Ref <- rep(stations$`@id`[i], dim(measures[[i]])[1])
#     measures[[i]] <- data.table::data.table(
#       rbind(
#         unlist(measures[[914]], recursive = FALSE)))
#     measures[[i]] <- measures[[i]][, lapply(.SD, as.character),]
#   }
# }
#
# for(i in seq_along(measures)){
#   if(is.null(dim(measures[[i]]))) next
#   measures[[i]]$Ref <- rep(stations$Ref[i], dim(measures[[i]])[1])
# }
#
# measures_dt <- rbindlist(measures, fill = TRUE)
# # Use regex to remove majority of URL
# measures_dt$`valueStatistic.@id` <- gsub(pattern = '(http[s]?://)?([^/s]+/)', x = measures_dt$`valueStatistic.@id`, replacement = '')
# colnames(measures_dt) <- c('ID', 'parameter', 'period', 'valueType', 'Ref')
#
# # Remove non required rows
# stationsClean <- data.table(stations[,-which(names(stations) %in% c('stationGuid','observedProperty','RLOIid','rloiStationLink', 'type', 'measures'))])
#
# #Merge tables full outer join
# all <- merge(stationsClean, measures_dt, all=TRUE)
# #Export
# write.csv(all, 'C:/Users/jpayne05/Desktop/API_all.csv')
#
