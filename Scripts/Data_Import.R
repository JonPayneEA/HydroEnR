library(HydroEnR)
library(sf)
library(leaflet)
library(htmltools)

# Link to files
link <- 'C:/Users/jpayne05/Desktop/RG_Analysis/RGs'
files <- list.files(link, full.names = TRUE)

# Import data
brigstock <- loadWISKI(files[1])
corby <- loadWISKI(files[2])
kingscliffe <- loadWISKI(files[3])
oundle <- loadWISKI(files[4])
wellingborough <- loadWISKI(files[5])
yelden <- loadWISKI(files[6])

profvis::profvis({
  oundle <- loadWISKI(files[4])
})

oundle$Metadata
oundle$GaugeData
oundle

# Collate the metadata
meta <- metaCollate(brigstock, corby, kingscliffe, oundle, wellingborough, yelden)
meta
gt::gt(meta) %>%
  tab_header(title = md('**Metadata for selected gauges**'),
             subtitle = md('Data are *uncleaned*')) %>%
  cols_label(Param_name = "Paramater", Param_type = "Type") %>%
  tab_source_note(source_note = "Source: WISKI") %>%
  tab_style(
    style = list(cell_fill(color = "#F4F4F4")),
    locations = cells_body(columns = c(Site, Area))
  )

# Import shapefile
link_shp <- 'C:/Users/jpayne05/Desktop/RG_Analysis/shp/FEH_Catchment_519250_298200.shp'
shape <- st_read(link_shp)

# Change projection to WGS 84
shape_t <- st_transform(shape, "+init=epsg:4326", coords)

# Get the coordinates for mapping
coords <- getCoords(brigstock, corby, kingscliffe, oundle, wellingborough, yelden)

# Generate Voronoi/Thiessen polygons
voronoi <- teeSun(coords, catchment = shape_t)

# Calculate intersections of voronoi and catchment
st_area(intersect(voronoi, shape_t, coords))

# teesunPlot
map <- teesunPlot(coords = coords, catchment = shape_t)
map

# Extract the proportions with
prop <- gaugeProp(coords, shape_t)
prop

# Plot some QA plots
# First we need to merge the datasets
merged_dt <- mergeData(corby, wellingborough, yelden, brigstock, kingscliffe, oundle)
merged_dt

# Create a meged data set
rain <- rainAvg(prop, merged_dt)
rain

# 1st QA plot - show missing data
plotNA(merged_dt)

# 2nd QA plot
# As it's interactive this plot will take a long time to render
# Aggregating to a daily resolution should improve efficiency
merged_daily <- dailyAgg(rain, method = 'sum')
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
