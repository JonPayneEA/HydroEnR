library(HydroEnR)

# Link to files
setwd("C:/Users/jpayne05/Desktop/RG_Analysis/RGs")
link <- 'C:/Users/jpayne05/Desktop/RG_Analysis/RGs/'
files <- list.files(link)

# Import data
brigstock <- loadWISKI(files[1])
corby <- loadWISKI(files[2])
kingscliffe <- loadWISKI(files[3])
oundle <- loadWISKI(files[4])
wellingborough <- loadWISKI(files[5])
yelden <- loadWISKI(files[6])

oundle$Metadata
oundle$GaugeData
oundle

# Collate the metadata
meta <- metaCollate(brigstock, corby, kingscliffe, oundle, wellingborough, yelden)
meta

# Get the coordinates for mapping
coords <- getCoords(brigstock, corby, kingscliffe, oundle, wellingborough, yelden)

# Generate Voronoi/Thiessen polygons
voronoi <- teeSun(coords)

leaflet(coords) %>%
  addTiles() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  # addProviderTiles(providers$CartoDB.Positron) %>%
  # addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addPolygons(data = voronoi,
              color = "blue",
              fillOpacity = 0.1,
              weight = 1.5) %>%
  addCircleMarkers(data = coords,
                   color = "orangered",
                   radius = 6,
                   fillOpacity = 0.8,
                   stroke = FALSE) %>%
  addMarkers(data = coords,
             popup = ~htmltools::htmlEscape(paste(coords$ID)))

# Plot some QA plots
# First we need to merge the datasets
merged_dt <- mergeData(brigstock, corby, kingscliffe, oundle, wellingborough, yelden)
merged_dt

# 1st QA plot - show missing data
plotNA(merged_dt)

# 2nd QA plot
# As it's interactive this plot will take a long time to render
# Aggregating to a daily resolution should improve efficiency
merged_daily <- dailyAgg(merged_dt, method = 'sum')
class(merged_daily)
plotQA(merged_daily)

# 3rd QA plot
# Create a cumulative sum data set
cumul <- cumsumNA(merged_daily)
plotQA(cumul)

# As the gauges have started recording at different times we need to partion or
# window the time series
merged_2009 <- window(merged_daily, start = '2009-10-01')
cumul_2009 <- cumsumNA(merged_2009)
plotQA(cumul_2009)
