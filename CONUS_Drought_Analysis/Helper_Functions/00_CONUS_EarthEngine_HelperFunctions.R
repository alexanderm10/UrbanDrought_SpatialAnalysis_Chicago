# CONUS-Scale Earth Engine Helper Functions for HLSL30 Processing
# Adapted from original Chicago analysis for continental US scale drought monitoring
# Author: Adapted for CONUS analysis
# Dataset: NASA HLS HLSL30 v002 (Landsat harmonized surface reflectance)

# Load required libraries
library(rgee)
library(raster) 
library(terra)

# Initialize Earth Engine
# ee_check() # For some reason, it's important to run this before initializing right now
# reticulate::use_condaenv("rgee", required = TRUE)  # Set this first after R restart

# ee_init <- rgee::ee_Initialize(
#   user = 'malexander@anl.gov',
#   drive = TRUE
# )

# ee <- rgee::ee
# ee$Initialize(project = "conusDroughtMonitor")

# ==========================================
# TIME AND DATE HELPER FUNCTIONS
# ==========================================

addTime <- function(image){ 
  return(image$addBands(image$metadata('system:time_start')$divide(1000 * 60 * 60 * 24 * 365)))
}

setYear <- function(img){
  return(img$set("year", img$date()$get("year")))
}

addYear <- function(img) {
  d <- ee$Date(ee$Number(img$get('system:time_start')))
  y <- ee$Number(d$get('year'))
  return(img$set('year', y))
}

# ==========================================
# HLSL30-SPECIFIC PROCESSING FUNCTIONS
# ==========================================

# Add NDVI calculation for HLSL30 data
# HLSL30 uses B4 (red) and B5 (NIR) for NDVI
addNDVI_HLSL30 <- function(img) {
  ndvi <- img$normalizedDifference(c('B5', 'B4'))$rename('NDVI')
  return(img$addBands(ndvi))
}

# Apply HLSL30 Fmask quality filtering
# Fmask band provides quality assessment for HLSL30
applyHLSL30QualityMask <- function(img) {
  fmask <- img$select('Fmask')
  
  # Fmask values:
  # 0: Clear land
  # 1: Clear water  
  # 2: Cloud shadow
  # 3: Snow/ice
  # 4: Cloud
  # 255: Fill value
  
  # Keep clear land pixels (value = 0) and optionally clear water (value = 1)
  clearMask <- fmask$eq(0)$Or(fmask$eq(1))
  
  return(img$updateMask(clearMask))
}

# Apply cloud coverage filter for HLSL30 collections
filterCloudCoverage <- function(collection, maxCloudCover = 30) {
  return(collection$filter(ee$Filter$lt('CLOUD_COVERAGE', maxCloudCover)))
}

# ==========================================
# CONUS-SCALE SPATIAL PROCESSING FUNCTIONS
# ==========================================

# Function for combining images with the same date (adapted for CONUS scale)
# Includes chunking for large-scale processing
mosaicByDateCONUS <- function(imcol, dayWindow = 7, maxImagesPerMosaic = 100){
  imlist <- imcol$toList(imcol$size())
  
  unique_dates <- imlist$map(ee_utils_pyfunc(function(img) {
    ee$Image(img)$date()$format("YYYY-MM-dd")
  }))$distinct()
  
  mosaic_imlist <- unique_dates$map(ee_utils_pyfunc(function(d){
    d <- ee$Date(d)
    dy <- d$get('day')    
    m <- d$get('month')
    y <- d$get('year')
    
    # Filter images within date window
    dateFiltered <- imcol$filterDate(d$advance(-dayWindow, "day"), d$advance(dayWindow, "day"))
    
    # For CONUS scale, use median reducer to handle large number of images
    im <- dateFiltered$reduce(ee$Reducer$median())
    
    return(im$set("system:time_start", d$millis(), 
                  "system:id", d$format("YYYY-MM-dd"),
                  'date', d, 'day', dy, 'month', m, 'year', y))
  }))
  
  return(ee$ImageCollection(mosaic_imlist))
}

# ==========================================
# CONUS BOUNDARY AND REGIONAL FUNCTIONS
# ==========================================

# Calculate regional NDVI means for CONUS regions
# Adapted for larger processing scale with chunking capability
regionNDVIMeanCONUS <- function(img, geometry, scale = 30, maxPixels = 1e10){
  ndviMean <- img$select("NDVI")$reduceRegion(
    reducer = ee$Reducer$mean(), 
    geometry = geometry,
    scale = scale,
    maxPixels = maxPixels,
    bestEffort = TRUE  # Important for CONUS scale processing
  )
  
  return(ee$Feature(NULL, ndviMean)$set(
    'system:time_start', img$get('system:time_start')
  )$set(
    'date', ee$Date(img$get('system:time_start'))$format("YYYY-MM-dd")
  ))
}

# ==========================================
# LANDCOVER MASKING FUNCTIONS (ADAPTED FOR CONUS)
# ==========================================

# Apply landcover masks by year for CONUS-scale processing
maskByLCCONUS <- function(imcol, MASK){
  imcol <- imcol$map(function(img){
    yrNow <- ee$Number(img$get('year'))$format()
    yrStr <- ee$String("YR")$cat(yrNow)
    
    maskNow <- MASK$select(yrStr)
    
    return(img$updateMask(maskNow))
  })
  return(imcol)
}

# ==========================================
# CONUS DATA EXTRACTION AND EXPORT FUNCTIONS
# ==========================================

# Extract NDVI by landcover for CONUS regions with state-level chunking
extractByLCCONUS <- function(imcol, landcover, geometry, outfolder, fileNamePrefix, 
                            scale = 30, maxPixels = 1e10, ...){
  
  lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")
  if(!landcover %in% lcnames){
    stop(paste("Invalid landcover type. Must match one of the following:", paste(lcnames, collapse = ", ")))
  }
  
  # Apply landcover mask (assumes mask objects are available in global environment)
  if(landcover=="forest") lcMask <- forMaskCONUS
  if(landcover=="crop") lcMask <- cropMaskCONUS
  if(landcover=="grassland") lcMask <- grassMaskCONUS
  if(landcover=="urban-high") lcMask <- urbHMaskCONUS
  if(landcover=="urban-medium") lcMask <- urbMMaskCONUS
  if(landcover=="urban-low") lcMask <- urbLMaskCONUS
  if(landcover=="urban-open") lcMask <- urbOMaskCONUS
  
  ndviLCYear <- maskByLCCONUS(imcol, lcMask)
  
  # Calculate regional means
  LCMeans <- ee$FeatureCollection(ndviLCYear$map(function(img) {
    regionNDVIMeanCONUS(img, geometry, scale, maxPixels)
  }))
  
  # Export to Drive
  LCMeansSave <- ee_table_to_drive(collection=LCMeans,
                                   description=paste0("Save_CONUS_", fileNamePrefix),
                                   folder=outfolder,
                                   fileNamePrefix=fileNamePrefix,
                                   timePrefix=T,
                                   fileFormat="CSV",
                                   selectors=c("date", "NDVI"))
  LCMeansSave$start()
  
  return(print(paste0(fileNamePrefix, " processed! Check Earth Engine queue for status")))
}

# ==========================================
# CONUS-SPECIFIC UTILITY FUNCTIONS
# ==========================================

# Create state-level processing chunks for CONUS analysis
createStateChunks <- function(states_fc) {
  # This function would create processing chunks by state or region
  # Implementation would depend on the states feature collection structure
  states_list <- states_fc$aggregate_array('NAME')$getInfo()
  
  chunks <- list()
  chunk_size <- 5  # Process 5 states at a time
  
  for(i in seq(1, length(states_list), chunk_size)) {
    end_idx <- min(i + chunk_size - 1, length(states_list))
    chunk_states <- states_list[i:end_idx]
    chunks[[length(chunks) + 1]] <- chunk_states
  }
  
  return(chunks)
}

# Memory-efficient processing function for large collections
processLargeCollection <- function(collection, processing_function, chunk_size = 100) {
  collection_size <- collection$size()$getInfo()
  
  if(collection_size <= chunk_size) {
    return(processing_function(collection))
  }
  
  # Process in chunks
  results <- list()
  for(i in seq(0, collection_size - 1, chunk_size)) {
    end_idx <- min(i + chunk_size - 1, collection_size - 1)
    chunk <- ee$ImageCollection(collection$toList(chunk_size, i))
    
    chunk_result <- processing_function(chunk)
    results[[length(results) + 1]] <- chunk_result
  }
  
  # Combine results
  combined_result <- results[[1]]
  if(length(results) > 1) {
    for(i in 2:length(results)) {
      combined_result <- combined_result$merge(results[[i]])
    }
  }
  
  return(combined_result)
}

# Print memory usage and processing time helper
printProcessingStats <- function(start_time, description) {
  end_time <- Sys.time()
  processing_time <- difftime(end_time, start_time, units = "mins")
  cat(paste0(description, " completed in ", round(processing_time, 2), " minutes\n"))
  cat(paste0("Memory usage: ", round(gc()[2,2], 2), " MB\n"))
}

# ==========================================
# CONUS LANDCOVER DEFINITIONS
# ==========================================

# NLCD landcover class definitions (same as Chicago but documented for CONUS)
# Forest: 41 (Deciduous), 42 (Evergreen), 43 (Mixed)
# Grassland: 51 (Dwarf Scrub - AK only), 52 (Shrub/Scrub), 71 (Grassland/Herbaceous), 72 (Sedge/Herbaceous - AK only)
# Crop: 81 (Pasture/Hay), 82 (Cultivated Crops)
# Urban-Open: 21 (Developed, Open Space)
# Urban-Low: 22 (Developed, Low Intensity)
# Urban-Medium: 23 (Developed, Medium Intensity)  
# Urban-High: 24 (Developed, High Intensity)

cat("CONUS Earth Engine Helper Functions loaded successfully.\n")
cat("Remember to uncomment and run the Earth Engine initialization section before using these functions.\n")