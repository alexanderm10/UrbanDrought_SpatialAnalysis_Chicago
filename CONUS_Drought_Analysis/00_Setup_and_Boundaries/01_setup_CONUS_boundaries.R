# CONUS Boundary Setup and NLCD Processing for Continental US Drought Analysis
# Purpose: Set up CONUS boundaries, NLCD landcover data, and create processing regions
# Author: Adapted for CONUS-scale analysis
# Dataset: NLCD 2019/2021, US States boundaries, CONUS extent

####################################################################################################################
# SETUP AND INITIALIZATION
####################################################################################################################

library(rgee)
library(raster) 
library(terra)
library(sf)

# Initialize Earth Engine
ee_check() # For some reason, it's important to run this before initializing right now
reticulate::use_condaenv("rgee", required = TRUE)  # Set this first after R restart

ee_init <- rgee::ee_Initialize(
  user = 'malexander@anl.gov',
  drive = TRUE
)

ee <- rgee::ee
ee$Initialize(project = "conusDroughtMonitor")

# Set up paths
assetHome <- ee_get_assethome()
path.google <- "~/Google Drive/My Drive/CONUS_DroughtAnalysis/"
path.google.share <- "~/Google Drive/Shared drives/Urban Ecological Drought/CONUS_Analysis/"

####################################################################################################################
# VISUALIZATION SETTINGS
####################################################################################################################

# Set center to continental US
Map$setCenter(-98.5795, 39.8283, 4)  # Geographic center of CONUS

# NLCD color palette
nlcdPalette <- c(
  '#5475A8', # Open Water (11)
  '#dec5c5', # Developed, Open Space (21)
  '#d99282', # Developed, Low Intensity (22)
  '#eb0000', # Developed, Medium Intensity (23)
  '#ab0000', # Developed High Intensity (24)
  '#b3ac9f', # Barren Land (31)
  '#68ab5f', # Deciduous Forest (41)
  '#1c5f2c', # Evergreen Forest (42)
  '#b5c58f', # Mixed Forest (43)
  '#ccb879', # Shrub/Scrub (52)
  '#dfdfc2', # Grassland/Herbaceous (71)
  '#dcd939', # Pasture/Hay (81)
  '#ab6c28', # Cultivated Crops (82)
  '#b8d9eb', # Woody Wetlands (90)
  '#6c9fb8'  # Emergent Herbaceous Wetlands (95)
)

nlcdvis <- list(
  min = 0,
  max = 95,
  palette = nlcdPalette
)

####################################################################################################################
# CONUS BOUNDARY DEFINITION
####################################################################################################################

# Option 1: Use US States to define CONUS (excludes AK, HI)
conus_states <- c('Alabama', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 
                  'Florida', 'Georgia', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 
                  'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 
                  'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 
                  'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 
                  'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 
                  'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming', 'District of Columbia')

# Load US States feature collection
US_States <- ee$FeatureCollection("TIGER/2018/States")

# Filter to CONUS states only
CONUS <- US_States$filter(ee$Filter$inList('NAME', conus_states))

# Create CONUS geometry
conus_bounds <- CONUS$geometry()$bounds()
conus_bbox <- ee$Geometry$BBox(-125, 25, -66, 49)  # Approximate CONUS bounding box

# Print CONUS info
ee_print(CONUS)
cat("CONUS boundary created with", length(conus_states), "states\n")

####################################################################################################################
# NLCD DATA PROCESSING FOR CONUS
####################################################################################################################

# Load NLCD 2019 collection for CONUS
nlcd_conus_2019 <- ee$ImageCollection('USGS/NLCD_RELEASES/2019_REL/NLCD')$
  select('landcover')$
  map(function(img){
    d <- ee$Date(ee$Number(img$get('system:time_start')))
    y <- ee$Number(d$get('year'))
    return(img$clip(conus_bounds)$set('year', y))
  })

# Load NLCD 2021 collection for CONUS  
nlcd_conus_2021 <- ee$ImageCollection('USGS/NLCD_RELEASES/2021_REL/NLCD')$
  select('landcover')$
  map(function(img){
    d <- ee$Date(ee$Number(img$get('system:time_start')))
    y <- ee$Number(d$get('year'))
    return(img$clip(conus_bounds)$set('year', y))
  })

# Visualize NLCD
Map$addLayer(nlcd_conus_2019$first()$select('landcover'), nlcdvis, 'NLCD 2019 CONUS')
Map$addLayer(nlcd_conus_2021$first()$select('landcover'), nlcdvis, 'NLCD 2021 CONUS')

# Get landcover value distribution
lcVals_conus <- nlcd_conus_2019$first()$reduceRegion(
  reducer = ee$Reducer$frequencyHistogram(), 
  geometry = conus_bounds, 
  scale = 300,  # Use coarser scale for initial assessment
  maxPixels = 1e10,
  bestEffort = TRUE
)

cat("NLCD landcover classes loaded for CONUS\n")

####################################################################################################################
# CREATE ANNUAL LANDCOVER LAYERS (2000-2024)
####################################################################################################################

# Extract specific years
lc_2001 <- nlcd_conus_2019$filter(ee$Filter$eq('system:index', '2001'))$first()
lc_2004 <- nlcd_conus_2019$filter(ee$Filter$eq('system:index', '2004'))$first()
lc_2006 <- nlcd_conus_2019$filter(ee$Filter$eq('system:index', '2006'))$first()
lc_2008 <- nlcd_conus_2019$filter(ee$Filter$eq('system:index', '2008'))$first()
lc_2011 <- nlcd_conus_2019$filter(ee$Filter$eq('system:index', '2011'))$first()
lc_2013 <- nlcd_conus_2019$filter(ee$Filter$eq('system:index', '2013'))$first()
lc_2016 <- nlcd_conus_2019$filter(ee$Filter$eq('system:index', '2016'))$first()
lc_2019 <- nlcd_conus_2019$filter(ee$Filter$eq('system:index', '2019'))$first()
lc_2021 <- nlcd_conus_2021$filter(ee$Filter$eq('system:index', '2021'))$first()

# Create duplicate layers for missing years (following Chicago methodology)
lc_2000 <- lc_2001$set('system:time_start', ee$Date$fromYMD(2000, 1, 1))$set('year', 2000)
lc_2002 <- lc_2001$set('system:time_start', ee$Date$fromYMD(2002, 1, 1))$set('year', 2002)
lc_2003 <- lc_2004$set('system:time_start', ee$Date$fromYMD(2003, 1, 1))$set('year', 2003)
lc_2005 <- lc_2006$set('system:time_start', ee$Date$fromYMD(2005, 1, 1))$set('year', 2005)
lc_2007 <- lc_2008$set('system:time_start', ee$Date$fromYMD(2007, 1, 1))$set('year', 2007)
lc_2009 <- lc_2008$set('system:time_start', ee$Date$fromYMD(2009, 1, 1))$set('year', 2009)
lc_2010 <- lc_2011$set('system:time_start', ee$Date$fromYMD(2010, 1, 1))$set('year', 2010)
lc_2012 <- lc_2013$set('system:time_start', ee$Date$fromYMD(2012, 1, 1))$set('year', 2012)
lc_2014 <- lc_2013$set('system:time_start', ee$Date$fromYMD(2014, 1, 1))$set('year', 2014)
lc_2015 <- lc_2016$set('system:time_start', ee$Date$fromYMD(2015, 1, 1))$set('year', 2015)
lc_2017 <- lc_2016$set('system:time_start', ee$Date$fromYMD(2017, 1, 1))$set('year', 2017)
lc_2018 <- lc_2019$set('system:time_start', ee$Date$fromYMD(2018, 1, 1))$set('year', 2018)
lc_2020 <- lc_2019$set('system:time_start', ee$Date$fromYMD(2020, 1, 1))$set('year', 2020)
lc_2022 <- lc_2021$set('system:time_start', ee$Date$fromYMD(2022, 1, 1))$set('year', 2022)
lc_2023 <- lc_2021$set('system:time_start', ee$Date$fromYMD(2023, 1, 1))$set('year', 2023)
lc_2024 <- lc_2021$set('system:time_start', ee$Date$fromYMD(2024, 1, 1))$set('year', 2024)
lc_2025 <- lc_2021$set('system:time_start', ee$Date$fromYMD(2025, 1, 1))$set('year', 2025)

# Combine into annual collection
collAnn_conus <- ee$ImageCollection(list(
  lc_2000, lc_2001, lc_2002, lc_2003, lc_2004, lc_2005, lc_2006, lc_2007, lc_2008, lc_2009,
  lc_2010, lc_2011, lc_2012, lc_2013, lc_2014, lc_2015, lc_2016, lc_2017, lc_2018, lc_2019,
  lc_2020, lc_2021, lc_2022, lc_2023, lc_2024, lc_2025
))

# Create year strings for band naming
yrLC_conus <- ee$List(collAnn_conus$aggregate_array("year"))$distinct()
yrString_conus <- ee$List(paste0("YR", yrLC_conus$getInfo()))

# Convert to multi-band image
lcCONUSAnn <- ee$ImageCollection$toBands(collAnn_conus)$rename(yrString_conus)

# Visualize one year
Map$addLayer(lcCONUSAnn$select("YR2020"), nlcdvis, 'NLCD CONUS 2020')

cat("Annual CONUS landcover collection created (2000-2025)\n")

####################################################################################################################
# PROCESSING REGIONS SETUP
####################################################################################################################

# Create regional processing divisions for memory management
# Option 1: By census regions
census_regions <- list(
  'Northeast' = c('Connecticut', 'Maine', 'Massachusetts', 'New Hampshire', 'Rhode Island', 'Vermont',
                  'New Jersey', 'New York', 'Pennsylvania'),
  'Midwest' = c('Illinois', 'Indiana', 'Michigan', 'Ohio', 'Wisconsin', 
                'Iowa', 'Kansas', 'Minnesota', 'Missouri', 'Nebraska', 'North Dakota', 'South Dakota'),
  'South' = c('Delaware', 'Florida', 'Georgia', 'Maryland', 'North Carolina', 'South Carolina', 'Virginia', 
              'District of Columbia', 'West Virginia', 'Alabama', 'Kentucky', 'Mississippi', 'Tennessee',
              'Arkansas', 'Louisiana', 'Oklahoma', 'Texas'),
  'West' = c('Arizona', 'Colorado', 'Idaho', 'Montana', 'Nevada', 'New Mexico', 'Utah', 'Wyoming',
             'California', 'Oregon', 'Washington')
)

# Create region feature collections
conus_regions <- list()
for(region_name in names(census_regions)) {
  region_fc <- US_States$filter(ee$Filter$inList('NAME', census_regions[[region_name]]))
  conus_regions[[region_name]] <- region_fc
  
  cat("Created region:", region_name, "with", length(census_regions[[region_name]]), "states\n")
}

####################################################################################################################
# EXPORT SETUP DATA TO ASSETS
####################################################################################################################

# Save CONUS boundary
save_conus_boundary <- ee_table_to_asset(
  collection = CONUS,
  description = "Save_CONUS_States_Boundary",
  assetId = file.path(assetHome, "CONUS_States_Boundary"),
  overwrite = TRUE
)
save_conus_boundary$start()

# Save annual landcover (this will be a large export - consider regional chunks)
save_conus_landcover <- ee_image_to_asset(
  image = lcCONUSAnn,
  description = "Save_NLCD_CONUS_Annual_2000-2025",
  assetId = file.path(assetHome, "NLCD_CONUS_Annual_2000-2025"),
  maxPixels = 1e12,
  scale = 30,
  region = conus_bounds,
  crs = "EPSG:4326",
  overwrite = TRUE
)
save_conus_landcover$start()

cat("CONUS boundary and landcover export tasks started\n")
cat("Check Earth Engine Tasks tab for processing status\n")

####################################################################################################################
# PROCESSING RECOMMENDATIONS AND NEXT STEPS
####################################################################################################################

cat("
CONUS Setup Complete!

Next Steps:
1. Wait for Earth Engine tasks to complete (check Tasks tab)
2. Run 02_create_CONUS_landcover_masks.R to create landcover-specific masks
3. Run HLSL30 data extraction scripts

Processing Notes:
- CONUS processing requires significant computational resources
- Consider regional processing (Northeast, Midwest, South, West)
- Use bestEffort=TRUE and appropriate maxPixels settings
- Monitor Earth Engine quotas during large exports

Asset Locations:
- CONUS States: ", file.path(assetHome, "CONUS_States_Boundary"), "
- NLCD Annual: ", file.path(assetHome, "NLCD_CONUS_Annual_2000-2025"), "
")

# Print processing statistics
cat("Processing completed at:", as.character(Sys.time()), "\n")