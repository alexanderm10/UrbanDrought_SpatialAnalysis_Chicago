# Acquire HLS (HLSL30) Data for CONUS Drought Analysis
# Alternative method using direct NASA Earthdata access
# Author: Local processing implementation

####################################################################################################################
# SETUP AND INITIALIZATION
####################################################################################################################

library(terra)
library(sf)
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(future)
library(furrr)

# Load local processing functions
source("../Helper_Functions/local_processing_functions.R")

# Setup parallel processing
setup_parallel(n_cores = 4)

####################################################################################################################
# CONFIGURATION
####################################################################################################################

# Data directories
data_dir <- "data/"
hls_dir <- file.path(data_dir, "hls_raw")
processed_dir <- file.path(data_dir, "hls_processed")
boundaries_dir <- file.path(data_dir, "boundaries")

# Create directories
for (dir in c(data_dir, hls_dir, processed_dir, boundaries_dir)) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

# CONUS bounding box
conus_bbox <- c(-125, 25, -66, 49)  # xmin, ymin, xmax, ymax

# Time range for analysis
date_start <- "2020-01-01"
date_end <- "2023-12-31"

# Processing parameters
max_cloud_cover <- 30
max_granules_per_search <- 2000

cat("Configuration set for CONUS HLS data acquisition\n")
cat("Bounding box:", paste(conus_bbox, collapse = ", "), "\n")
cat("Date range:", date_start, "to", date_end, "\n")

####################################################################################################################
# NASA EARTHDATA AUTHENTICATION
####################################################################################################################

# NOTE: Users need to set up NASA Earthdata credentials
# Register at: https://urs.earthdata.nasa.gov/

# Set up authentication (replace with your credentials)
# setup_earthdata_auth("your_username", "your_password")

# For this example, we'll use environment variables
if (Sys.getenv("EARTHDATA_USERNAME") == "" || Sys.getenv("EARTHDATA_PASSWORD") == "") {
  cat("
NASA Earthdata credentials not found.
Please set environment variables or use:
setup_earthdata_auth('your_username', 'your_password')

To register: https://urs.earthdata.nasa.gov/
  ")
  stop("NASA Earthdata authentication required")
}

cat("NASA Earthdata authentication configured\n")

####################################################################################################################
# SEARCH AND CATALOG HLS DATA
####################################################################################################################

# Search for HLS granules
cat("Searching for HLS granules...\n")

hls_granules <- search_hls_data(
  bbox = conus_bbox,
  date_start = date_start,
  date_end = date_end,
  cloud_cover = max_cloud_cover,
  max_results = max_granules_per_search
)

if (nrow(hls_granules) == 0) {
  stop("No HLS granules found for the specified criteria")
}

cat("Found", nrow(hls_granules), "HLS granules\n")

# Add processed date column
hls_granules <- hls_granules %>%
  mutate(
    date = as.Date(substr(time_start, 1, 10)),
    year = year(date),
    month = month(date),
    yday = yday(date)
  ) %>%
  arrange(date)

# Summary by year
yearly_summary <- hls_granules %>%
  group_by(year) %>%
  summarise(
    n_granules = n(),
    mean_cloud_cover = mean(cloud_cover, na.rm = TRUE),
    date_range = paste(min(date), "to", max(date))
  )

print(yearly_summary)

####################################################################################################################
# SAMPLE DOWNLOAD FOR TESTING
####################################################################################################################

# For initial testing, download a small sample
sample_size <- 10
sample_granules <- hls_granules %>%
  slice_sample(n = sample_size) %>%
  arrange(date)

cat("Starting sample download of", sample_size, "granules...\n")

# Download sample granules
sample_downloads <- future_map(1:nrow(sample_granules), function(i) {
  granule <- sample_granules[i, ]
  
  tryCatch({
    filepath <- download_hls_granule(granule, hls_dir)
    
    list(
      granule_id = granule$granule_id,
      date = granule$date,
      filepath = filepath,
      success = !is.na(filepath),
      error = NA
    )
  }, error = function(e) {
    list(
      granule_id = granule$granule_id,
      date = granule$date,
      filepath = NA,
      success = FALSE,
      error = as.character(e)
    )
  })
}, .progress = TRUE)

# Summarize download results
download_results <- bind_rows(sample_downloads)
successful_downloads <- sum(download_results$success, na.rm = TRUE)

cat("Successfully downloaded", successful_downloads, "out of", sample_size, "granules\n")

if (successful_downloads == 0) {
  stop("No successful downloads. Check NASA Earthdata credentials and network connection.")
}

####################################################################################################################
# PROCESS SAMPLE GRANULES
####################################################################################################################

# Process downloaded HLS files to NDVI
cat("Processing downloaded HLS granules to NDVI...\n")

successful_files <- download_results$filepath[download_results$success & !is.na(download_results$filepath)]

if (length(successful_files) > 0) {
  
  # Process files in parallel
  ndvi_files <- future_map(successful_files, function(hdf_file) {
    
    tryCatch({
      ndvi_file <- process_hls_granule(hdf_file, processed_dir)
      
      list(
        input_file = hdf_file,
        ndvi_file = ndvi_file,
        success = TRUE,
        error = NA
      )
    }, error = function(e) {
      list(
        input_file = hdf_file,
        ndvi_file = NA,
        success = FALSE,
        error = as.character(e)
      )
    })
  }, .progress = TRUE)
  
  # Summarize processing results
  processing_results <- bind_rows(ndvi_files)
  successful_processing <- sum(processing_results$success, na.rm = TRUE)
  
  cat("Successfully processed", successful_processing, "NDVI files\n")
  
  if (successful_processing > 0) {
    
    # List successful NDVI files
    ndvi_file_paths <- processing_results$ndvi_file[processing_results$success & !is.na(processing_results$ndvi_file)]
    
    cat("Sample NDVI files created:\n")
    for (file in head(ndvi_file_paths, 5)) {
      cat(" -", basename(file), "\n")
    }
    
    # Quick visualization of first NDVI file
    if (length(ndvi_file_paths) > 0) {
      sample_ndvi <- rast(ndvi_file_paths[1])
      cat("Sample NDVI statistics:\n")
      cat("  Min:", minmax(sample_ndvi)[1], "\n")
      cat("  Max:", minmax(sample_ndvi)[2], "\n")
      cat("  Mean:", global(sample_ndvi, "mean", na.rm = TRUE)[1,1], "\n")
    }
    
  } else {
    cat("No successful NDVI processing. Check HDF file formats and processing functions.\n")
  }
  
} else {
  cat("No successful downloads to process.\n")
}

####################################################################################################################
# BATCH PROCESSING SETUP
####################################################################################################################

# Function to process granules in batches
process_hls_batch <- function(granule_batch, batch_id) {
  
  cat("Processing batch", batch_id, "with", nrow(granule_batch), "granules\n")
  
  # Download batch
  download_results <- future_map(1:nrow(granule_batch), function(i) {
    granule <- granule_batch[i, ]
    
    tryCatch({
      filepath <- download_hls_granule(granule, hls_dir)
      return(filepath)
    }, error = function(e) {
      cat("Download error for", granule$granule_id, ":", as.character(e), "\n")
      return(NA)
    })
  })
  
  # Process successful downloads
  successful_files <- unlist(download_results[!is.na(download_results)])
  
  if (length(successful_files) > 0) {
    
    ndvi_results <- future_map(successful_files, function(hdf_file) {
      
      tryCatch({
        ndvi_file <- process_hls_granule(hdf_file, processed_dir)
        return(ndvi_file)
      }, error = function(e) {
        cat("Processing error for", basename(hdf_file), ":", as.character(e), "\n")
        return(NA)
      })
    })
    
    successful_ndvi <- unlist(ndvi_results[!is.na(ndvi_results)])
    
    cat("Batch", batch_id, "completed:", length(successful_ndvi), "NDVI files created\n")
    
    return(successful_ndvi)
    
  } else {
    cat("Batch", batch_id, "failed: No successful downloads\n")
    return(character(0))
  }
}

# Create processing batches (for full processing)
batch_size <- 50
n_batches <- ceiling(nrow(hls_granules) / batch_size)

cat("Full processing would require", n_batches, "batches of", batch_size, "granules each\n")

####################################################################################################################
# FULL PROCESSING EXECUTION (OPTIONAL)
####################################################################################################################

# Uncomment and run this section for full CONUS processing
if (FALSE) {  # Set to TRUE to run full processing
  
  all_ndvi_files <- character(0)
  
  for (batch_id in 1:n_batches) {
    
    # Create batch
    start_idx <- (batch_id - 1) * batch_size + 1
    end_idx <- min(batch_id * batch_size, nrow(hls_granules))
    batch_granules <- hls_granules[start_idx:end_idx, ]
    
    # Process batch
    batch_ndvi_files <- process_hls_batch(batch_granules, batch_id)
    all_ndvi_files <- c(all_ndvi_files, batch_ndvi_files)
    
    # Save progress
    saveRDS(all_ndvi_files, file.path(processed_dir, "all_ndvi_files.rds"))
    
    # Add delay between batches to be respectful to servers
    if (batch_id < n_batches) {
      cat("Waiting 60 seconds before next batch...\n")
      Sys.sleep(60)
    }
  }
  
  cat("Full processing completed:", length(all_ndvi_files), "total NDVI files\n")
  
} else {
  cat("Full processing disabled. Set conditional to TRUE to run all", n_batches, "batches.\n")
}

####################################################################################################################
# DATA CATALOG CREATION
####################################################################################################################

# Create metadata catalog for processed files
ndvi_files <- list.files(processed_dir, pattern = "NDVI\\.tif$", full.names = TRUE)

if (length(ndvi_files) > 0) {
  
  catalog <- tibble(
    ndvi_file = ndvi_files,
    filename = basename(ndvi_files),
    file_size_mb = file.size(ndvi_files) / 1024^2
  )
  
  # Extract date information from filenames (this depends on HLS naming convention)
  catalog <- catalog %>%
    mutate(
      # Extract date from HLS filename (adjust regex as needed)
      date_str = str_extract(filename, "\\d{7}"),  # Julian date format
      # Convert to actual date (this is approximate - refine based on actual HLS naming)
      date = as.Date(paste0(substr(date_str, 1, 4), "-01-01")) + as.numeric(substr(date_str, 5, 7)) - 1
    ) %>%
    arrange(date)
  
  # Save catalog
  catalog_file <- file.path(processed_dir, "ndvi_catalog.csv")
  write_csv(catalog, catalog_file)
  
  cat("Data catalog created with", nrow(catalog), "NDVI files\n")
  cat("Catalog saved to:", catalog_file, "\n")
  
  # Summary statistics
  cat("Date range:", min(catalog$date, na.rm = TRUE), "to", max(catalog$date, na.rm = TRUE), "\n")
  cat("Total data size:", round(sum(catalog$file_size_mb, na.rm = TRUE) / 1024, 2), "GB\n")
  
} else {
  cat("No NDVI files found for catalog creation\n")
}

####################################################################################################################
# NEXT STEPS AND RECOMMENDATIONS
####################################################################################################################

cat("
HLS Data Acquisition Complete!

Completed Steps:
1. Configured NASA Earthdata access
2. Searched and cataloged available HLS granules
3. Downloaded and processed sample data
4. Created NDVI files from HLS surface reflectance

Next Steps:
1. Run landcover processing: 02_process_landcover_masks.R
2. Extract time series by landcover type: 03_extract_timeseries.R
3. Analyze drought indicators: 04_drought_analysis.R

Full Processing Notes:
- Total granules found: ", nrow(hls_granules), "
- Estimated download size: Several hundred GB to TB
- Processing time: Hours to days depending on hardware
- Consider regional processing to manage data volumes

Data Storage:
- Raw HLS files: ", hls_dir, "
- Processed NDVI: ", processed_dir, "
- Consider cloud storage for large datasets

Performance Tips:
- Use SSD storage for better I/O performance
- Increase parallel workers for faster processing
- Monitor disk space during large downloads
- Implement checkpointing for long-running processes
")

cat("Data acquisition script completed at:", as.character(Sys.time()), "\n")