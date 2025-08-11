# CONUS Drought Analysis - Local Processing Setup (No GEE Required)
# Alternative implementation using direct data access and local processing
# Author: Adapted for non-GEE processing

####################################################################################################################
# REQUIRED PACKAGES FOR LOCAL PROCESSING
####################################################################################################################

# Core spatial processing
library(terra)      # Modern raster processing
library(sf)         # Vector data handling
library(stars)      # Spatiotemporal arrays

# Data acquisition
library(httr)       # HTTP requests for APIs
library(jsonlite)   # JSON parsing
library(aws.s3)     # AWS data access (if using)

# Time series processing
library(lubridate)  # Date handling
library(dplyr)      # Data manipulation
library(purrr)      # Functional programming

# Parallel processing
library(future)     # Parallel/async processing
library(furrr)      # Parallel purrr
library(doParallel) # Parallel processing backend

# Optional: Cloud computing interfaces
# library(reticulate) # Python integration for Planetary Computer
# library(rstac)      # STAC API interface

cat("Local processing packages loaded successfully.\n")

####################################################################################################################
# CONUS BOUNDARY SETUP
####################################################################################################################

# Define CONUS states (excluding AK, HI)
conus_states <- c('Alabama', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 
                  'Florida', 'Georgia', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 
                  'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 
                  'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 
                  'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 
                  'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 
                  'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming', 'District of Columbia')

# CONUS bounding box
conus_bbox <- c(xmin = -125, ymin = 25, xmax = -66, ymax = 49)

# Create CONUS extent
conus_extent <- ext(conus_bbox)

cat("CONUS boundary defined: ", length(conus_states), " states\n")
cat("CONUS bounding box: ", paste(conus_bbox, collapse = ", "), "\n")

####################################################################################################################
# DATA ACQUISITION FUNCTIONS
####################################################################################################################

# Function to download US states boundaries
get_conus_boundaries <- function() {
  
  # Download from Census Bureau (alternative to TIGER/GEE)
  states_url <- "https://www2.census.gov/geo/tiger/GENZ2021/shp/cb_2021_us_state_20m.zip"
  
  temp_dir <- tempdir()
  zip_file <- file.path(temp_dir, "us_states.zip")
  
  download.file(states_url, zip_file, mode = "wb")
  unzip(zip_file, exdir = temp_dir)
  
  # Read and filter to CONUS
  states_sf <- st_read(file.path(temp_dir, "cb_2021_us_state_20m.shp"))
  conus_sf <- states_sf[states_sf$NAME %in% conus_states, ]
  
  return(conus_sf)
}

# Function to download NLCD data
get_nlcd_data <- function(year = 2021, data_dir = "data/nlcd/") {
  
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  
  # NLCD download URLs
  nlcd_urls <- list(
    "2021" = "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2021_land_cover_l48_20230630.zip",
    "2019" = "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2019_land_cover_l48_20210604.zip",
    "2016" = "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2016_land_cover_l48_20190424.zip"
  )
  
  if (!as.character(year) %in% names(nlcd_urls)) {
    stop("Year must be one of: ", paste(names(nlcd_urls), collapse = ", "))
  }
  
  url <- nlcd_urls[[as.character(year)]]
  zip_file <- file.path(data_dir, paste0("nlcd_", year, ".zip"))
  
  if (!file.exists(zip_file)) {
    cat("Downloading NLCD", year, "data...\n")
    download.file(url, zip_file, mode = "wb")
  }
  
  # Extract if needed
  extract_dir <- file.path(data_dir, paste0("nlcd_", year))
  if (!dir.exists(extract_dir)) {
    unzip(zip_file, exdir = extract_dir)
  }
  
  # Find and return the main raster file
  tif_files <- list.files(extract_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
  main_file <- tif_files[grepl("land_cover", tif_files)][1]
  
  return(main_file)
}

# Function to access Landsat data from AWS
get_landsat_aws <- function(path, row, date, data_dir = "data/landsat/") {
  
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  
  # Construct AWS S3 path
  date_formatted <- format(as.Date(date), "%Y/%m/%d")
  collection <- "02"
  level <- "L2"
  
  # Example AWS path structure
  s3_path <- sprintf("s3://usgs-landsat/collection%s/level-%s/standard/oli-tirs/%s/%03d/%03d/",
                     collection, level, gsub("-", "/", substr(date, 1, 10)), path, row)
  
  # This would require AWS CLI or aws.s3 package configuration
  # Implementation depends on specific AWS setup
  
  cat("AWS S3 path constructed: ", s3_path, "\n")
  return(s3_path)
}

# Function to search Microsoft Planetary Computer (requires Python setup)
search_planetary_computer <- function(bbox, date_range, collection = "landsat-c2-l2") {
  
  # This requires reticulate and pystac-client Python package
  if (!reticulate::py_module_available("pystac_client")) {
    stop("Python pystac_client package required. Install with: pip install pystac-client planetary-computer")
  }
  
  py_run_string("
import pystac_client
import planetary_computer as pc

catalog = pystac_client.Client.open(
    'https://planetarycomputer.microsoft.com/api/stac/v1',
    modifier=pc.sign_inplace
)
")
  
  # Search for data
  py_run_string(sprintf("
search = catalog.search(
    collections=['%s'],
    bbox=[%s],
    datetime='%s'
)
items = list(search.get_items())
", collection, paste(bbox, collapse = ","), paste(date_range, collapse = "/")))
  
  items <- py$items
  return(items)
}

####################################################################################################################
# LOCAL RASTER PROCESSING FUNCTIONS
####################################################################################################################

# Calculate NDVI for Landsat data
calculate_ndvi_landsat <- function(red_band, nir_band) {
  
  # Load bands
  red <- rast(red_band)
  nir <- rast(nir_band)
  
  # Calculate NDVI
  ndvi <- (nir - red) / (nir + red)
  
  # Set valid range
  ndvi[ndvi < -1 | ndvi > 1] <- NA
  
  return(ndvi)
}

# Apply quality masks for Landsat Collection 2
apply_landsat_quality_mask <- function(image_file, qa_band_file) {
  
  # Load QA band
  qa <- rast(qa_band_file)
  
  # Landsat Collection 2 QA_PIXEL bit definitions
  # Bit 0: Fill
  # Bit 1: Dilated Cloud
  # Bit 2: Cirrus
  # Bit 3: Cloud
  # Bit 4: Cloud Shadow
  # Bit 5: Snow
  # Bit 6: Clear
  # Bit 7: Water
  
  # Create clear land mask (bit 6 = 1, bits 1,3,4,5 = 0)
  clear_mask <- bitwAnd(qa, 64) > 0  # Bit 6: Clear
  cloud_mask <- bitwAnd(qa, 8) == 0  # Bit 3: Cloud
  shadow_mask <- bitwAnd(qa, 16) == 0  # Bit 4: Cloud shadow
  snow_mask <- bitwAnd(qa, 32) == 0   # Bit 5: Snow
  
  final_mask <- clear_mask & cloud_mask & shadow_mask & snow_mask
  
  # Load and mask image
  img <- rast(image_file)
  img_masked <- mask(img, final_mask, maskvalue = FALSE)
  
  return(img_masked)
}

# Create landcover masks from NLCD
create_landcover_masks <- function(nlcd_file, output_dir = "data/masks/") {
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  nlcd <- rast(nlcd_file)
  
  # Define landcover classes
  lc_classes <- list(
    forest = c(41, 42, 43),
    grassland = c(52, 71),
    crop = c(81, 82),
    urban_open = c(21),
    urban_low = c(22),
    urban_medium = c(23),
    urban_high = c(24)
  )
  
  masks <- list()
  
  for (lc_name in names(lc_classes)) {
    cat("Creating", lc_name, "mask...\n")
    
    # Create binary mask
    mask_values <- lc_classes[[lc_name]]
    lc_mask <- nlcd %in% mask_values
    
    # Save mask
    mask_file <- file.path(output_dir, paste0("nlcd_", lc_name, "_mask.tif"))
    writeRaster(lc_mask, mask_file, overwrite = TRUE)
    
    masks[[lc_name]] <- lc_mask
  }
  
  return(masks)
}

####################################################################################################################
# PARALLEL PROCESSING SETUP
####################################################################################################################

# Setup parallel processing
setup_parallel_processing <- function(n_cores = NULL) {
  
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores() - 1
  }
  
  # Setup future for parallel processing
  plan(multisession, workers = n_cores)
  
  # Setup doParallel for other parallel operations
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  cat("Parallel processing setup with", n_cores, "cores\n")
  
  return(cl)
}

# Process multiple raster files in parallel
process_rasters_parallel <- function(file_list, process_function, ...) {
  
  results <- future_map(file_list, process_function, ..., .progress = TRUE)
  
  return(results)
}

####################################################################################################################
# TEMPORAL AGGREGATION FUNCTIONS
####################################################################################################################

# Create temporal composites (e.g., weekly, monthly)
create_temporal_composite <- function(raster_list, dates, composite_period = "week") {
  
  # Convert dates
  dates <- as.Date(dates)
  
  # Create temporal grouping
  if (composite_period == "week") {
    groups <- lubridate::week(dates)
  } else if (composite_period == "month") {
    groups <- lubridate::month(dates)
  } else if (composite_period == "year") {
    groups <- lubridate::year(dates)
  }
  
  # Group rasters by time period
  unique_groups <- unique(groups)
  composites <- list()
  
  for (group in unique_groups) {
    group_indices <- which(groups == group)
    group_rasters <- raster_list[group_indices]
    
    if (length(group_rasters) > 1) {
      # Create stack and calculate median
      stack_rasters <- rast(group_rasters)
      composite <- app(stack_rasters, fun = median, na.rm = TRUE)
    } else {
      composite <- rast(group_rasters[[1]])
    }
    
    composites[[as.character(group)]] <- composite
  }
  
  return(composites)
}

####################################################################################################################
# NEXT STEPS AND IMPLEMENTATION GUIDE
####################################################################################################################

cat("
Local Processing Setup Complete!

Implementation Steps:
1. Download CONUS boundaries: conus_sf <- get_conus_boundaries()
2. Download NLCD data: nlcd_file <- get_nlcd_data(2021)
3. Create landcover masks: masks <- create_landcover_masks(nlcd_file)
4. Setup parallel processing: cl <- setup_parallel_processing()
5. Acquire satellite data (Landsat/MODIS) through chosen method
6. Process NDVI time series using local functions

Data Storage Recommendations:
- Use file-based databases (e.g., SQLite, DuckDB) for metadata
- Store rasters in Cloud Optimized GeoTIFF (COG) format
- Implement tiling strategy for memory-efficient processing
- Use compression to reduce storage requirements

Performance Considerations:
- Process by tiles/regions to manage memory
- Use terra/stars for efficient raster operations  
- Implement caching for intermediate results
- Consider using external storage (AWS S3, Google Cloud Storage)

Alternative Data Sources:
1. NASA Earthdata: Direct API access
2. Microsoft Planetary Computer: Free tier available
3. AWS Open Data: Landsat, MODIS, Sentinel-2
4. USGS Earth Explorer: Manual/automated downloads
5. Copernicus Open Access Hub: Sentinel data
")

cat("Local processing framework ready at:", as.character(Sys.time()), "\n")