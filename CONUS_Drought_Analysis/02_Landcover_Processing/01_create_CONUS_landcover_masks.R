# Create CONUS Landcover Masks for Drought Analysis
# Purpose: Generate landcover-specific masks for CONUS region using NLCD data
# Author: Adapted for CONUS-scale analysis from Chicago workflow

####################################################################################################################
# SETUP AND INITIALIZATION
####################################################################################################################

library(rgee)
library(raster)
library(terra)

# Initialize Earth Engine
ee_check() # For some reason, it's important to run this before initializing right now
reticulate::use_condaenv("rgee", required = TRUE)  # Set this first after R restart

ee_init <- rgee::ee_Initialize(
  user = 'malexander@anl.gov',
  drive = TRUE
)

ee <- rgee::ee
ee$Initialize(project = "conusDroughtMonitor")

# Load helper functions
source("../Helper_Functions/00_CONUS_EarthEngine_HelperFunctions.R")

# Set up paths
assetHome <- ee_get_assethome()

####################################################################################################################
# LOAD CONUS BOUNDARIES AND ANNUAL LANDCOVER
####################################################################################################################

# Load CONUS boundary and annual landcover (created in setup script)
CONUS <- ee$FeatureCollection(file.path(assetHome, "CONUS_States_Boundary"))
conus_bounds <- CONUS$geometry()$bounds()

# Load annual landcover collection
lcCONUSAnn <- ee$Image(file.path(assetHome, "NLCD_CONUS_Annual_2000-2025"))

cat("CONUS boundaries and annual landcover loaded\n")

# Verify landcover data
nlcdvis <- list(min = 0, max = 95, palette = c('#5475A8', '#dec5c5', '#d99282', '#eb0000', '#ab0000', 
                                               '#b3ac9f', '#68ab5f', '#1c5f2c', '#b5c58f', '#ccb879', 
                                               '#dfdfc2', '#dcd939', '#ab6c28', '#b8d9eb', '#6c9fb8'))
Map$setCenter(-98.5795, 39.8283, 4)
Map$addLayer(lcCONUSAnn$select("YR2020"), nlcdvis, 'CONUS Landcover 2020')

####################################################################################################################
# CREATE LANDCOVER-SPECIFIC MASKS
####################################################################################################################

# Recreate annual collection from multi-band image for mask creation
years <- 2000:2025
year_strings <- paste0("YR", years)

# Convert back to collection for processing
annual_images <- list()
for(i in 1:length(years)) {
  year <- years[i]
  year_str <- year_strings[i]
  
  img <- lcCONUSAnn$select(year_str)$rename('landcover')$
    set('system:time_start', ee$Date$fromYMD(year, 1, 1)$millis())$
    set('year', year)
  
  annual_images[[i]] <- img
}

collAnn_conus <- ee$ImageCollection(annual_images)

cat("Annual collection recreated with", length(annual_images), "images\n")

####################################################################################################################
# FOREST MASK: NLCD Classes 41, 42, 43
####################################################################################################################

cat("Creating forest mask...\n")

classFor_conus <- collAnn_conus$map(function(image) {
  d <- ee$Date(ee$Number(image$get('system:time_start')))
  lcMask <- image$select('landcover')$eq(41)$
    Or(image$select('landcover')$eq(42))$
    Or(image$select('landcover')$eq(43))
  
  return(image$updateMask(lcMask)$set('class', 'Forest')$set('year', d$get('year')))
})

forMask_conus <- ee$ImageCollection$toBands(classFor_conus)$rename(year_strings)

# Export forest mask
saveForMask_conus <- ee_image_to_asset(
  image = forMask_conus,
  description = "Save_CONUS_lcMask_Forest_2000-2025",
  assetId = file.path(assetHome, "NLCD_CONUS_2000-2025_Forest"),
  maxPixels = 1e12,
  scale = 30,
  region = conus_bounds,
  crs = "EPSG:4326",
  overwrite = TRUE
)
saveForMask_conus$start()
cat("Forest mask export started\n")

####################################################################################################################
# GRASSLAND MASK: NLCD Classes 52, 71 (excluding AK-only classes 51, 72)
####################################################################################################################

cat("Creating grassland mask...\n")

classGrass_conus <- collAnn_conus$map(function(image) {
  d <- ee$Date(ee$Number(image$get('system:time_start')))
  lcMask <- image$select('landcover')$eq(52)$
    Or(image$select('landcover')$eq(71))
  
  return(image$updateMask(lcMask)$set('class', 'Grassland')$set('year', d$get('year')))
})

grassMask_conus <- ee$ImageCollection$toBands(classGrass_conus)$rename(year_strings)

# Export grassland mask
saveGrassMask_conus <- ee_image_to_asset(
  image = grassMask_conus,
  description = "Save_CONUS_lcMask_Grassland_2000-2025",
  assetId = file.path(assetHome, "NLCD_CONUS_2000-2025_Grassland"),
  maxPixels = 1e12,
  scale = 30,
  region = conus_bounds,
  crs = "EPSG:4326",
  overwrite = TRUE
)
saveGrassMask_conus$start()
cat("Grassland mask export started\n")

####################################################################################################################
# CROP MASK: NLCD Classes 81, 82
####################################################################################################################

cat("Creating crop mask...\n")

classCrop_conus <- collAnn_conus$map(function(image) {
  d <- ee$Date(ee$Number(image$get('system:time_start')))
  lcMask <- image$select('landcover')$eq(81)$
    Or(image$select('landcover')$eq(82))
  
  return(image$updateMask(lcMask)$set('class', 'Crop')$set('year', d$get('year')))
})

cropMask_conus <- ee$ImageCollection$toBands(classCrop_conus)$rename(year_strings)

# Export crop mask
saveCropMask_conus <- ee_image_to_asset(
  image = cropMask_conus,
  description = "Save_CONUS_lcMask_Crop_2000-2025",
  assetId = file.path(assetHome, "NLCD_CONUS_2000-2025_Crop"),
  maxPixels = 1e12,
  scale = 30,
  region = conus_bounds,
  crs = "EPSG:4326",
  overwrite = TRUE
)
saveCropMask_conus$start()
cat("Crop mask export started\n")

####################################################################################################################
# URBAN OPEN MASK: NLCD Class 21
####################################################################################################################

cat("Creating urban-open mask...\n")

classUrbO_conus <- collAnn_conus$map(function(image) {
  d <- ee$Date(ee$Number(image$get('system:time_start')))
  lcMask <- image$select('landcover')$eq(21)
  
  return(image$updateMask(lcMask)$set('class', 'Urban-Open')$set('year', d$get('year')))
})

urbOMask_conus <- ee$ImageCollection$toBands(classUrbO_conus)$rename(year_strings)

# Export urban-open mask
saveUrbOMask_conus <- ee_image_to_asset(
  image = urbOMask_conus,
  description = "Save_CONUS_lcMask_Urban-Open_2000-2025",
  assetId = file.path(assetHome, "NLCD_CONUS_2000-2025_Urban-Open"),
  maxPixels = 1e12,
  scale = 30,
  region = conus_bounds,
  crs = "EPSG:4326",
  overwrite = TRUE
)
saveUrbOMask_conus$start()
cat("Urban-Open mask export started\n")

####################################################################################################################
# URBAN LOW MASK: NLCD Class 22
####################################################################################################################

cat("Creating urban-low mask...\n")

classUrbL_conus <- collAnn_conus$map(function(image) {
  d <- ee$Date(ee$Number(image$get('system:time_start')))
  lcMask <- image$select('landcover')$eq(22)
  
  return(image$updateMask(lcMask)$set('class', 'Urban-Low')$set('year', d$get('year')))
})

urbLMask_conus <- ee$ImageCollection$toBands(classUrbL_conus)$rename(year_strings)

# Export urban-low mask
saveUrbLMask_conus <- ee_image_to_asset(
  image = urbLMask_conus,
  description = "Save_CONUS_lcMask_Urban-Low_2000-2025",
  assetId = file.path(assetHome, "NLCD_CONUS_2000-2025_Urban-Low"),
  maxPixels = 1e12,
  scale = 30,
  region = conus_bounds,
  crs = "EPSG:4326",
  overwrite = TRUE
)
saveUrbLMask_conus$start()
cat("Urban-Low mask export started\n")

####################################################################################################################
# URBAN MEDIUM MASK: NLCD Class 23
####################################################################################################################

cat("Creating urban-medium mask...\n")

classUrbM_conus <- collAnn_conus$map(function(image) {
  d <- ee$Date(ee$Number(image$get('system:time_start')))
  lcMask <- image$select('landcover')$eq(23)
  
  return(image$updateMask(lcMask)$set('class', 'Urban-Medium')$set('year', d$get('year')))
})

urbMMask_conus <- ee$ImageCollection$toBands(classUrbM_conus)$rename(year_strings)

# Export urban-medium mask
saveUrbMMask_conus <- ee_image_to_asset(
  image = urbMMask_conus,
  description = "Save_CONUS_lcMask_Urban-Medium_2000-2025",
  assetId = file.path(assetHome, "NLCD_CONUS_2000-2025_Urban-Medium"),
  maxPixels = 1e12,
  scale = 30,
  region = conus_bounds,
  crs = "EPSG:4326",
  overwrite = TRUE
)
saveUrbMMask_conus$start()
cat("Urban-Medium mask export started\n")

####################################################################################################################
# URBAN HIGH MASK: NLCD Class 24
####################################################################################################################

cat("Creating urban-high mask...\n")

classUrbH_conus <- collAnn_conus$map(function(image) {
  d <- ee$Date(ee$Number(image$get('system:time_start')))
  lcMask <- image$select('landcover')$eq(24)
  
  return(image$updateMask(lcMask)$set('class', 'Urban-High')$set('year', d$get('year')))
})

urbHMask_conus <- ee$ImageCollection$toBands(classUrbH_conus)$rename(year_strings)

# Export urban-high mask
saveUrbHMask_conus <- ee_image_to_asset(
  image = urbHMask_conus,
  description = "Save_CONUS_lcMask_Urban-High_2000-2025",
  assetId = file.path(assetHome, "NLCD_CONUS_2000-2025_Urban-High"),
  maxPixels = 1e12,
  scale = 30,
  region = conus_bounds,
  crs = "EPSG:4326",
  overwrite = TRUE
)
saveUrbHMask_conus$start()
cat("Urban-High mask export started\n")

####################################################################################################################
# PROCESSING SUMMARY AND NEXT STEPS
####################################################################################################################

cat("
CONUS Landcover Mask Creation Complete!

Export Tasks Started:
1. Forest (NLCD: 41, 42, 43)
2. Grassland (NLCD: 52, 71) 
3. Crop (NLCD: 81, 82)
4. Urban-Open (NLCD: 21)
5. Urban-Low (NLCD: 22)
6. Urban-Medium (NLCD: 23)
7. Urban-High (NLCD: 24)

Asset Locations:
")

asset_names <- c("Forest", "Grassland", "Crop", "Urban-Open", "Urban-Low", "Urban-Medium", "Urban-High")
for(asset in asset_names) {
  cat("-", file.path(assetHome, paste0("NLCD_CONUS_2000-2025_", asset)), "\n")
}

cat("
Processing Notes:
- Each export task may take 2-8 hours for CONUS scale
- Total data size will be substantial (several GB per mask)
- Monitor Earth Engine quotas and processing limits
- Tasks can be monitored at: https://code.earthengine.google.com/tasks

Next Steps:
1. Wait for all mask creation tasks to complete
2. Run HLSL30 data extraction script
3. Verify mask quality by sampling across CONUS regions

Processing completed at:", as.character(Sys.time()), "\n")

# Check current tasks
cat("Check Earth Engine Tasks tab for current progress\n")