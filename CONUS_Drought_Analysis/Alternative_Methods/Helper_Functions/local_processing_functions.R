# Local Processing Helper Functions for CONUS Drought Analysis
# Alternative to Google Earth Engine processing
# Author: Local processing implementation

####################################################################################################################
# REQUIRED PACKAGES
####################################################################################################################

library(terra)      # Modern raster processing
library(sf)         # Vector data
library(stars)      # Spatiotemporal arrays
library(httr)       # HTTP requests
library(jsonlite)   # JSON parsing
library(lubridate)  # Date handling
library(dplyr)      # Data manipulation
library(future)     # Parallel processing
library(furrr)      # Parallel purrr

####################################################################################################################
# DATA ACQUISITION FUNCTIONS
####################################################################################################################

# NASA Earthdata authentication
setup_earthdata_auth <- function(username, password) {
  # Store credentials for NASA Earthdata
  Sys.setenv(EARTHDATA_USERNAME = username)
  Sys.setenv(EARTHDATA_PASSWORD = password)
  
  cat("NASA Earthdata credentials configured\n")
}

# Search HLSL30 data via CMR API
search_hls_data <- function(bbox, date_start, date_end, cloud_cover = 30, max_results = 2000) {
  
  base_url <- "https://cmr.earthdata.nasa.gov/search/granules.json"
  
  # Convert bbox to CMR format
  bbox_str <- paste(bbox[c(1,3,2,4)], collapse = ",")  # xmin,xmax,ymin,ymax
  
  params <- list(
    collection_concept_id = "C2021957295-LPCLOUD",  # HLSL30 v002
    bounding_box = bbox_str,
    temporal = paste0(date_start, ",", date_end),
    cloud_cover = paste0("0,", cloud_cover),
    page_size = min(max_results, 2000)
  )
  
  response <- GET(base_url, query = params)
  
  if (status_code(response) == 200) {
    data <- content(response, "parsed")
    
    # Extract relevant information
    granules <- data$feed$entry
    
    if (length(granules) > 0) {
      results <- map_dfr(granules, function(granule) {
        
        # Extract download links
        links <- granule$links
        data_links <- keep(links, ~ .$rel == "http://esipfed.org/ns/fedsearch/1.1/data#")
        
        tibble(
          granule_id = granule$id,
          title = granule$title %||% NA,
          time_start = granule$time_start %||% NA,
          time_end = granule$time_end %||% NA,
          cloud_cover = as.numeric(granule$cloud_cover %||% NA),
          download_url = if(length(data_links) > 0) data_links[[1]]$href else NA
        )
      })
      
      cat("Found", nrow(results), "HLS granules\n")
      return(results)
      
    } else {
      cat("No granules found for the specified criteria\n")
      return(tibble())
    }
    
  } else {
    stop("API request failed with status: ", status_code(response))
  }
}

# Download HLS data files
download_hls_granule <- function(granule_info, download_dir = "data/hls/") {
  
  if (!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)
  
  # Create filename from granule ID
  filename <- paste0(gsub("[^A-Za-z0-9_.-]", "_", granule_info$granule_id), ".hdf")
  filepath <- file.path(download_dir, filename)
  
  if (!file.exists(filepath)) {
    cat("Downloading:", granule_info$title, "\n")
    
    # Download with authentication
    username <- Sys.getenv("EARTHDATA_USERNAME")
    password <- Sys.getenv("EARTHDATA_PASSWORD")
    
    response <- GET(
      granule_info$download_url,
      authenticate(username, password),
      write_disk(filepath),
      progress()
    )
    
    if (status_code(response) == 200) {
      cat("Downloaded successfully:", filename, "\n")
      return(filepath)
    } else {
      cat("Download failed for:", filename, "\n")
      return(NA)
    }
  } else {
    cat("File already exists:", filename, "\n")
    return(filepath)
  }
}

# Microsoft Planetary Computer interface
search_planetary_computer <- function(bbox, date_start, date_end, collection = "landsat-c2-l2", 
                                     cloud_cover = 30) {
  
  # Requires Python environment with pystac-client
  if (!reticulate::py_module_available("pystac_client")) {
    stop("Python pystac_client required: pip install pystac-client planetary-computer")
  }
  
  reticulate::py_run_string("
import pystac_client
import planetary_computer as pc
from datetime import datetime

# Connect to Planetary Computer
catalog = pystac_client.Client.open(
    'https://planetarycomputer.microsoft.com/api/stac/v1',
    modifier=pc.sign_inplace
)
")
  
  # Format parameters for Python
  bbox_py <- paste0("[", paste(bbox, collapse = ","), "]")
  datetime_py <- paste0(date_start, "/", date_end)
  
  py_code <- sprintf("
# Search for items
search = catalog.search(
    collections=['%s'],
    bbox=%s,
    datetime='%s',
    query={'eo:cloud_cover': {'lt': %d}}
)

items = list(search.get_items())
print(f'Found {len(items)} items')
", collection, bbox_py, datetime_py, cloud_cover)
  
  reticulate::py_run_string(py_code)
  
  # Get results
  items <- reticulate::py$items
  
  if (length(items) > 0) {
    cat("Found", length(items), "items from Planetary Computer\n")
  }
  
  return(items)
}

####################################################################################################################
# RASTER PROCESSING FUNCTIONS
####################################################################################################################

# Extract and process HLS bands
process_hls_granule <- function(hdf_file, output_dir = "data/processed/") {
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Get subdatasets from HDF file
  sds_info <- describe(hdf_file, sds = TRUE)
  
  # Extract relevant bands (B04=Red, B05=NIR for HLSL30)
  red_band <- NULL
  nir_band <- NULL
  qa_band <- NULL
  
  for (sds in sds_info$subdatasets) {
    if (grepl("B04", sds)) red_band <- sds
    if (grepl("B05", sds)) nir_band <- sds
    if (grepl("Fmask", sds)) qa_band <- sds
  }
  
  if (is.null(red_band) || is.null(nir_band)) {
    stop("Required bands not found in HDF file")
  }
  
  # Load bands
  red <- rast(red_band)
  nir <- rast(nir_band)
  
  # Apply quality mask if available
  if (!is.null(qa_band)) {
    qa <- rast(qa_band)
    # HLS Fmask: 0=clear land, 1=clear water, 2=cloud shadow, 3=snow/ice, 4=cloud
    clear_mask <- qa %in% c(0, 1)  # Keep clear land and water
    red <- mask(red, clear_mask, maskvalue = FALSE)
    nir <- mask(nir, clear_mask, maskvalue = FALSE)
  }
  
  # Calculate NDVI
  ndvi <- (nir - red) / (nir + red)
  ndvi[ndvi < -1 | ndvi > 1] <- NA
  
  # Create output filename
  base_name <- tools::file_path_sans_ext(basename(hdf_file))
  ndvi_file <- file.path(output_dir, paste0(base_name, "_NDVI.tif"))
  
  # Save NDVI
  writeRaster(ndvi, ndvi_file, overwrite = TRUE)
  
  return(ndvi_file)
}

# Process Landsat data (for AWS or direct downloads)
process_landsat_scene <- function(scene_dir, output_dir = "data/processed/") {
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Find required files in scene directory
  scene_files <- list.files(scene_dir, full.names = TRUE)
  
  # Landsat Collection 2 band naming
  red_file <- scene_files[grepl("_SR_B4\\.TIF$", scene_files, ignore.case = TRUE)]
  nir_file <- scene_files[grepl("_SR_B5\\.TIF$", scene_files, ignore.case = TRUE)]
  qa_file <- scene_files[grepl("_QA_PIXEL\\.TIF$", scene_files, ignore.case = TRUE)]
  
  if (length(red_file) == 0 || length(nir_file) == 0) {
    stop("Required Landsat bands not found in scene directory")
  }
  
  # Load bands
  red <- rast(red_file)
  nir <- rast(nir_file)
  
  # Apply scaling factors for Collection 2
  red <- red * 0.0000275 + (-0.2)
  nir <- nir * 0.0000275 + (-0.2)
  
  # Apply quality mask
  if (length(qa_file) > 0) {
    qa <- rast(qa_file)
    
    # Quality mask for clear land pixels
    clear_mask <- bitwAnd(qa, 64) > 0    # Bit 6: Clear
    cloud_mask <- bitwAnd(qa, 8) == 0    # Bit 3: Not cloud
    shadow_mask <- bitwAnd(qa, 16) == 0  # Bit 4: Not cloud shadow
    
    final_mask <- clear_mask & cloud_mask & shadow_mask
    red <- mask(red, final_mask, maskvalue = FALSE)
    nir <- mask(nir, final_mask, maskvalue = FALSE)
  }
  
  # Calculate NDVI
  ndvi <- (nir - red) / (nir + red)
  ndvi[ndvi < -1 | ndvi > 1] <- NA
  
  # Save NDVI
  scene_id <- basename(scene_dir)
  ndvi_file <- file.path(output_dir, paste0(scene_id, "_NDVI.tif"))
  writeRaster(ndvi, ndvi_file, overwrite = TRUE)
  
  return(ndvi_file)
}

####################################################################################################################
# LANDCOVER PROCESSING FUNCTIONS
####################################################################################################################

# Download and process NLCD data
get_nlcd_conus <- function(year = 2021, data_dir = "data/nlcd/") {
  
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  
  # NLCD URLs
  nlcd_urls <- list(
    "2021" = "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2021_land_cover_l48_20230630.zip",
    "2019" = "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2019_land_cover_l48_20210604.zip",
    "2016" = "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2016_land_cover_l48_20190424.zip"
  )
  
  year_str <- as.character(year)
  if (!year_str %in% names(nlcd_urls)) {
    stop("Year must be one of: ", paste(names(nlcd_urls), collapse = ", "))
  }
  
  zip_file <- file.path(data_dir, paste0("nlcd_", year, ".zip"))
  extract_dir <- file.path(data_dir, year_str)
  
  # Download if needed
  if (!file.exists(zip_file)) {
    cat("Downloading NLCD", year, "data...\n")
    download.file(nlcd_urls[[year_str]], zip_file, mode = "wb")
  }
  
  # Extract if needed
  if (!dir.exists(extract_dir)) {
    cat("Extracting NLCD data...\n")
    unzip(zip_file, exdir = extract_dir)
  }
  
  # Find the landcover raster
  tif_files <- list.files(extract_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
  nlcd_file <- tif_files[grepl("land_cover", basename(tif_files))][1]
  
  return(nlcd_file)
}

# Create landcover masks
create_landcover_masks_local <- function(nlcd_file, conus_boundary, output_dir = "data/masks/") {
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Load NLCD and clip to CONUS
  cat("Loading NLCD data...\n")
  nlcd <- rast(nlcd_file)
  
  if (!is.null(conus_boundary)) {
    nlcd <- crop(nlcd, conus_boundary)
    nlcd <- mask(nlcd, conus_boundary)
  }
  
  # Define landcover classes (same as GEE version)
  lc_classes <- list(
    forest = c(41, 42, 43),
    grassland = c(52, 71),  # Excluding AK-only classes
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
    lc_mask <- nlcd %in% lc_classes[[lc_name]]
    
    # Save mask
    mask_file <- file.path(output_dir, paste0("conus_", lc_name, "_mask.tif"))
    writeRaster(lc_mask, mask_file, overwrite = TRUE)
    
    masks[[lc_name]] <- mask_file
  }
  
  cat("Landcover masks created in:", output_dir, "\n")
  return(masks)
}

####################################################################################################################
# TIME SERIES PROCESSING FUNCTIONS
####################################################################################################################

# Extract NDVI time series by landcover
extract_ndvi_timeseries <- function(ndvi_files, dates, landcover_mask, region_boundary = NULL) {
  
  results <- tibble()
  
  for (i in seq_along(ndvi_files)) {
    
    cat("Processing", basename(ndvi_files[i]), "\n")
    
    # Load NDVI
    ndvi <- rast(ndvi_files[i])
    
    # Crop to region if specified
    if (!is.null(region_boundary)) {
      ndvi <- crop(ndvi, region_boundary)
      ndvi <- mask(ndvi, region_boundary)
    }
    
    # Apply landcover mask
    lc_mask <- rast(landcover_mask)
    
    # Ensure same extent and resolution
    if (!compareGeom(ndvi, lc_mask, stopOnError = FALSE)) {
      lc_mask <- resample(lc_mask, ndvi, method = "near")
    }
    
    ndvi_masked <- mask(ndvi, lc_mask, maskvalue = FALSE)
    
    # Calculate regional mean
    ndvi_mean <- global(ndvi_masked, fun = "mean", na.rm = TRUE)[1,1]
    
    # Store result
    result <- tibble(
      date = dates[i],
      ndvi = ndvi_mean,
      file = basename(ndvi_files[i])
    )
    
    results <- bind_rows(results, result)
  }
  
  return(results)
}

# Create temporal composites
create_temporal_composites <- function(ndvi_files, dates, period = "month", output_dir = "data/composites/") {
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Create temporal grouping
  dates <- as.Date(dates)
  
  if (period == "week") {
    groups <- paste0(year(dates), "_W", week(dates))
  } else if (period == "month") {
    groups <- paste0(year(dates), "_", sprintf("%02d", month(dates)))
  } else if (period == "year") {
    groups <- as.character(year(dates))
  }
  
  unique_groups <- unique(groups)
  composite_files <- character(length(unique_groups))
  
  for (i in seq_along(unique_groups)) {
    group <- unique_groups[i]
    group_indices <- which(groups == group)
    group_files <- ndvi_files[group_indices]
    
    cat("Creating composite for", group, "with", length(group_files), "files\n")
    
    if (length(group_files) > 1) {
      # Load all rasters for the period
      ndvi_stack <- rast(group_files)
      
      # Calculate median composite
      composite <- app(ndvi_stack, fun = median, na.rm = TRUE)
    } else {
      composite <- rast(group_files[1])
    }
    
    # Save composite
    composite_file <- file.path(output_dir, paste0("ndvi_composite_", group, ".tif"))
    writeRaster(composite, composite_file, overwrite = TRUE)
    
    composite_files[i] <- composite_file
  }
  
  return(tibble(
    period = unique_groups,
    composite_file = composite_files
  ))
}

####################################################################################################################
# UTILITY FUNCTIONS
####################################################################################################################

# Setup parallel processing
setup_parallel <- function(n_cores = NULL) {
  
  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)
  }
  
  plan(multisession, workers = n_cores)
  cat("Parallel processing setup with", n_cores, "cores\n")
  
  return(n_cores)
}

# Get CONUS state boundaries
get_conus_states <- function(data_dir = "data/boundaries/") {
  
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  
  # Try to read existing file first
  states_file <- file.path(data_dir, "conus_states.gpkg")
  
  if (file.exists(states_file)) {
    cat("Loading existing CONUS states boundary\n")
    return(st_read(states_file, quiet = TRUE))
  }
  
  # Download from Census Bureau
  states_url <- "https://www2.census.gov/geo/tiger/GENZ2021/shp/cb_2021_us_state_20m.zip"
  
  temp_dir <- tempdir()
  zip_file <- file.path(temp_dir, "us_states.zip")
  
  cat("Downloading US states boundaries...\n")
  download.file(states_url, zip_file, mode = "wb", quiet = TRUE)
  unzip(zip_file, exdir = temp_dir)
  
  # Read and filter to CONUS
  states_sf <- st_read(file.path(temp_dir, "cb_2021_us_state_20m.shp"), quiet = TRUE)
  
  conus_states <- c('Alabama', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 
                    'Florida', 'Georgia', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 
                    'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 
                    'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 
                    'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 
                    'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 
                    'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming', 'District of Columbia')
  
  conus_sf <- states_sf[states_sf$NAME %in% conus_states, ]
  
  # Save for future use
  st_write(conus_sf, states_file, quiet = TRUE)
  
  cat("CONUS states boundary loaded:", nrow(conus_sf), "states\n")
  return(conus_sf)
}

cat("Local processing helper functions loaded successfully!\n")