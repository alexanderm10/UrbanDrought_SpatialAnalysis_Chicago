# HLSL30 NDVI Data Extraction for CONUS Drought Analysis
# Purpose: Extract HLSL30 (Landsat) harmonized surface reflectance data for CONUS
# Dataset: NASA HLS HLSL30 v002 - Landsat harmonized surface reflectance
# Author: Adapted for CONUS-scale drought analysis

####################################################################################################################
# SETUP AND INITIALIZATION  
####################################################################################################################

library(rgee)
library(raster)
library(terra)
library(lubridate)

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
path.google <- "~/Google Drive/My Drive/CONUS_DroughtAnalysis/"
NDVIsave <- "My Drive/CONUS_DroughtAnalysis/HLSL30_Extracts"

####################################################################################################################
# VISUALIZATION AND SETTINGS
####################################################################################################################

# Set center to CONUS
Map$setCenter(-98.5795, 39.8283, 4)

# NDVI visualization parameters
ndviVis <- list(
  min = 0.0,
  max = 1.0,
  palette = c(
    '#FFFFFF', '#CE7E45', '#DF923D', '#F1B555', '#FCD163', '#99B718', '#74A901',
    '#66A000', '#529400', '#3E8601', '#207401', '#056201', '#004C00', '#023B01',
    '#012E01', '#011D01', '#011301'
  )
)

####################################################################################################################
# LOAD CONUS BOUNDARIES AND LANDCOVER MASKS
####################################################################################################################

# Load CONUS boundary (created in previous script)
CONUS <- ee$FeatureCollection(file.path(assetHome, "CONUS_States_Boundary"))
conus_bounds <- CONUS$geometry()$bounds()

# Load landcover masks (these would be created by separate landcover mask script)
# For now, we'll define the asset paths - these need to be created first
forMaskCONUS <- ee$Image(file.path(assetHome, "NLCD_CONUS_2000-2025_Forest"))
grassMaskCONUS <- ee$Image(file.path(assetHome, "NLCD_CONUS_2000-2025_Grassland"))
cropMaskCONUS <- ee$Image(file.path(assetHome, "NLCD_CONUS_2000-2025_Crop"))
urbOMaskCONUS <- ee$Image(file.path(assetHome, "NLCD_CONUS_2000-2025_Urban-Open"))
urbLMaskCONUS <- ee$Image(file.path(assetHome, "NLCD_CONUS_2000-2025_Urban-Low"))
urbMMaskCONUS <- ee$Image(file.path(assetHome, "NLCD_CONUS_2000-2025_Urban-Medium"))
urbHMaskCONUS <- ee$Image(file.path(assetHome, "NLCD_CONUS_2000-2025_Urban-High"))

cat("Landcover masks loaded for CONUS processing\n")

####################################################################################################################
# HLSL30 DATA COLLECTION AND PROCESSING
####################################################################################################################

# Define processing parameters
start_date <- "2013-04-25"  # HLSL30 start date
end_date <- "2024-12-31"    # Current processing end date
max_cloud_cover <- 30       # Maximum cloud coverage percentage

cat("Processing HLSL30 data from", start_date, "to", end_date, "\n")

# Load HLSL30 collection
hlsl30_raw <- ee$ImageCollection("NASA/HLS/HLSL30/v002")$
  filterBounds(conus_bounds)$
  filterDate(start_date, end_date)$
  filter(ee$Filter$lt('CLOUD_COVERAGE', max_cloud_cover))

cat("Raw HLSL30 collection size:", hlsl30_raw$size()$getInfo(), "images\n")

# Apply processing functions
hlsl30_processed <- hlsl30_raw$map(function(img) {
  # Extract date components
  d <- ee$Date(img$get('system:time_start'))
  dy <- d$get('day')    
  m <- d$get('month')
  y <- d$get('year')
  
  # Clip to CONUS bounds
  img_clipped <- img$clip(conus_bounds)
  
  # Apply HLSL30 quality mask
  img_masked <- applyHLSL30QualityMask(img_clipped)
  
  # Add NDVI band
  img_ndvi <- addNDVI_HLSL30(img_masked)
  
  return(img_ndvi$set('date', d, 'day', dy, 'month', m, 'year', y))
})

# Select relevant bands (B4, B5 for verification, NDVI for analysis)
hlsl30_ndvi <- hlsl30_processed$select(c('B4', 'B5', 'NDVI'))

cat("Processed HLSL30 collection size:", hlsl30_ndvi$size()$getInfo(), "images\n")

# Create daily mosaics to reduce processing load
hlsl30_mosaic <- mosaicByDateCONUS(hlsl30_ndvi, dayWindow = 7)$
  select(c('B4_median', 'B5_median', 'NDVI_median'), c('B4', 'B5', 'NDVI'))$
  sort("date")

cat("Mosaiced HLSL30 collection size:", hlsl30_mosaic$size()$getInfo(), "images\n")

# Visualize sample image
sample_img <- hlsl30_mosaic$first()
Map$addLayer(sample_img$select('NDVI'), ndviVis, 'HLSL30 NDVI Sample')

####################################################################################################################
# REGIONAL PROCESSING APPROACH
####################################################################################################################

# Define census regions for processing (to manage computational load)
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

# Load US States for regional filtering
US_States <- ee$FeatureCollection("TIGER/2018/States")

####################################################################################################################
# EXTRACT NDVI BY LANDCOVER AND REGION
####################################################################################################################

# Define landcover types to process
lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

# Function to process a specific region and landcover type
processRegionLC <- function(region_name, region_states, landcover_type) {
  
  cat("Processing", region_name, "-", landcover_type, "\n")
  
  # Create region geometry
  region_fc <- US_States$filter(ee$Filter$inList('NAME', region_states))
  region_geom <- region_fc$geometry()
  
  # Filter HLSL30 data to region
  hlsl30_region <- hlsl30_mosaic$map(function(img) {
    img$clip(region_geom)
  })
  
  # Apply landcover mask
  if(landcover_type == "forest") lcMask <- forMaskCONUS
  if(landcover_type == "crop") lcMask <- cropMaskCONUS
  if(landcover_type == "grassland") lcMask <- grassMaskCONUS
  if(landcover_type == "urban-high") lcMask <- urbHMaskCONUS
  if(landcover_type == "urban-medium") lcMask <- urbMMaskCONUS
  if(landcover_type == "urban-low") lcMask <- urbLMaskCONUS
  if(landcover_type == "urban-open") lcMask <- urbOMaskCONUS
  
  hlsl30_masked <- maskByLCCONUS(hlsl30_region, lcMask)
  
  # Calculate regional means
  region_means <- ee$FeatureCollection(hlsl30_masked$map(function(img) {
    regionNDVIMeanCONUS(img, region_geom, scale = 30, maxPixels = 1e10)
  }))
  
  # Export to Drive
  file_prefix <- paste0("HLSL30_", region_name, "_", landcover_type)
  
  export_task <- ee_table_to_drive(
    collection = region_means,
    description = paste0("Save_", file_prefix),
    folder = NDVIsave,
    fileNamePrefix = file_prefix,
    timePrefix = TRUE,
    fileFormat = "CSV",
    selectors = c("date", "NDVI")
  )
  
  export_task$start()
  
  return(paste0(file_prefix, " export started"))
}

####################################################################################################################
# BATCH PROCESSING EXECUTION
####################################################################################################################

# Option 1: Process all regions and landcover types (WARNING: This creates many tasks!)
if(FALSE) {  # Set to TRUE to run full processing
  
  results <- list()
  
  for(region_name in names(census_regions)) {
    for(lc_type in lcnames) {
      
      result <- processRegionLC(region_name, census_regions[[region_name]], lc_type)
      results <- append(results, result)
      
      # Add delay to prevent overwhelming Earth Engine
      Sys.sleep(2)
    }
  }
  
  cat("All processing tasks submitted:", length(results), "total tasks\n")
  cat("Check Earth Engine Tasks tab for progress\n")
}

####################################################################################################################
# ALTERNATIVE: SINGLE REGION/LANDCOVER TESTING
####################################################################################################################

# For initial testing, process just one region/landcover combination
test_region <- "Northeast"
test_landcover <- "forest"

cat("Starting test processing:", test_region, "-", test_landcover, "\n")

# Process test case
test_result <- processRegionLC(test_region, census_regions[[test_region]], test_landcover)
cat(test_result, "\n")

####################################################################################################################
# CONUS-WIDE PROCESSING (ADVANCED)
####################################################################################################################

# Alternative approach: Process entire CONUS for each landcover type
# WARNING: This requires significant computational resources
processCONUSLC <- function(landcover_type) {
  
  cat("Processing CONUS-wide", landcover_type, "\n")
  
  # Apply landcover mask
  if(landcover_type == "forest") lcMask <- forMaskCONUS
  if(landcover_type == "crop") lcMask <- cropMaskCONUS
  if(landcover_type == "grassland") lcMask <- grassMaskCONUS
  if(landcover_type == "urban-high") lcMask <- urbHMaskCONUS
  if(landcover_type == "urban-medium") lcMask <- urbMMaskCONUS
  if(landcover_type == "urban-low") lcMask <- urbLMaskCONUS
  if(landcover_type == "urban-open") lcMask <- urbOMaskCONUS
  
  hlsl30_masked <- maskByLCCONUS(hlsl30_mosaic, lcMask)
  
  # Calculate CONUS-wide means
  conus_means <- ee$FeatureCollection(hlsl30_masked$map(function(img) {
    regionNDVIMeanCONUS(img, conus_bounds, scale = 30, maxPixels = 1e11)
  }))
  
  # Export to Drive
  file_prefix <- paste0("HLSL30_CONUS_", landcover_type)
  
  export_task <- ee_table_to_drive(
    collection = conus_means,
    description = paste0("Save_", file_prefix),
    folder = NDVIsave,
    fileNamePrefix = file_prefix,
    timePrefix = TRUE,
    fileFormat = "CSV",
    selectors = c("date", "NDVI")
  )
  
  export_task$start()
  
  return(paste0(file_prefix, " export started"))
}

# Uncomment to run CONUS-wide processing for all landcover types
# for(lc_type in lcnames) {
#   result <- processCONUSLC(lc_type)
#   cat(result, "\n")
#   Sys.sleep(5)  # Delay between submissions
# }

####################################################################################################################
# MONITORING AND VALIDATION
####################################################################################################################

# Function to check processing progress
checkTasks <- function() {
  cat("Check your Earth Engine Tasks tab at: https://code.earthengine.google.com/tasks\n")
  cat("Tasks may take several hours to complete for CONUS-scale processing\n")
}

# Print memory usage
gc_info <- gc()
cat("Current memory usage:", round(gc_info[2,2], 2), "MB\n")

####################################################################################################################
# NEXT STEPS AND RECOMMENDATIONS
####################################################################################################################

cat("
HLSL30 Processing Setup Complete!

Recommended Processing Order:
1. Test with single region/landcover combination first
2. Verify data quality and processing parameters
3. Run regional processing (4 regions x 7 landcover types = 28 tasks)
4. Consider CONUS-wide processing only after regional validation

Processing Notes:
- Each task may take 1-6 hours depending on data volume
- Monitor Earth Engine quotas and processing limits
- Use regional approach to manage computational resources
- Verify landcover masks exist before running extraction

Data Outputs:
- CSV files saved to: ", NDVIsave, "
- Each file contains date and NDVI columns
- Files can be combined for temporal analysis

Next Script: 02_process_HLSL30_outputs.R for data compilation and analysis
")

cat("Processing script completed at:", as.character(Sys.time()), "\n")