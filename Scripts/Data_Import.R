library(HydroEnR)
library(leaflet)
library(sf)
library(htmltools)
library(data.table)
library(dygraphs)
library(gt)
library(ggplot2)
library(RcppRoll)

# Link to files
link <- 'C:/Users/jpayne05/Desktop/RG_Analysis/RGs'
files <- list.files(link, full.names = TRUE)
files

# Import data
brigstock <- loadWISKI(files[1])
corby <- loadWISKI(files[2])
kingscliffe <- loadWISKI(files[3])
oundle <- loadWISKI(files[4])
wellingborough <- loadWISKI(files[5])
yelden <- loadWISKI(files[6])

# Optional
profvis::profvis({
  oundle <- loadWISKI(files[4])
})

oundle
oundle$Metadata
oundle$GaugeData

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
coords

# Generate Voronoi/Thiessen polygons
voronoi <- teeSun(coords, catchment = shape_t)
voronoi

# Calculate intersections of voronoi and catchment
st_area(intersectPoly(voronoi, shape_t, coords))

# Extract the proportions with
prop <- gaugeProp(coords, shape_t)
prop

# teesunPlot
map <- teesunPlot(coords = coords, catchment = shape_t)
map

# Plot some QA plots
# First we need to merge the datasets
merged_dt <- mergeData(corby, wellingborough, yelden, brigstock, kingscliffe, oundle)
merged_dt

# Create a meged data set
rain <- rainAvg(prop, merged_dt)
rain

# 1st QA plot - show missing data
plotNA(merged_dt)
plotNA(rain)

# 2nd QA plot
# As it's interactive this plot will take a long time to render
# Aggregating to a daily resolution should improve efficiency
merged_daily <- dailyAgg(rain, method = 'sum')
plotQA(merged_daily)

# 3rd QA plot
# Create a cumulative sum data set
cumul <- cumsumNA(merged_daily)
plotQA(cumul)

# As the gauges have started recording at different times we need to partition or
# window the time series
merged_2009 <- window(merged_daily, start = '2009-10-01')
cumul_2009 <- cumsumNA(merged_2009)
plotQA(cumul_2009)

# Loading PE data
pe_link <- ('C:/Users/jpayne05/OneDrive - Defra/R/Packages/Test_data/PE/PE.csv')
dt <- loadPE(pe_link)
dt
dt$RemoteData$PE

# Plot of the data with a smoother of type GAM
ggplot(dt$RemoteData, aes(x = DateTime, y = PE)) +
  geom_line() +
  ggtitle('PE data for River Lee at Silver Street') +
  xlab('Time') +
  ylab('mm day') +
  geom_smooth(colour = 'red', size = 1.5)

# Load flow data
flow_link <- 'C:/Users/jpayne05/Desktop/Buildwas_15min_Flow.csv'
buildwas <- loadWISKI(flow_link)
buildwas
asVol.flowLoad(buildwas)

hourlyAgg(buildwas, method = 'min')
dailyAgg(buildwas)
monthlyAgg(buildwas, method = 'sum')
annualAgg(buildwas)
hydroYearAgg(buildwas, method = 'max')
rollingAggs(buildwas, method = 'max')

oundle_vol <- asVol.rainLoad(oundle)
prop
oundle_vol <- asVol.rainLoad(oundle, area_km = 219)
oundle_vol
