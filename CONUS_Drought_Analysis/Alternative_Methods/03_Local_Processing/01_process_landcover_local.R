# Process Landcover Data Locally for CONUS Drought Analysis
# Alternative method using direct NLCD data access
# Author: Local processing implementation

####################################################################################################################
# SETUP AND INITIALIZATION
####################################################################################################################

library(terra)
library(sf)
library(dplyr)
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
nlcd_dir <- file.path(data_dir, "nlcd")
masks_dir <- file.path(data_dir, "landcover_masks")
boundaries_dir <- file.path(data_dir, "boundaries")

# Create directories
for (dir in c(nlcd_dir, masks_dir, boundaries_dir)) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

# NLCD years to process
nlcd_years <- c(2021, 2019, 2016)

cat("Starting landcover processing for CONUS\n")
cat("NLCD years:", paste(nlcd_years, collapse = ", "), "\n")

####################################################################################################################
# ACQUIRE CONUS BOUNDARIES
####################################################################################################################

# Get CONUS state boundaries
cat("Loading CONUS state boundaries...\n")
conus_states <- get_conus_states(boundaries_dir)

# Create unified CONUS boundary
conus_boundary <- st_union(conus_states)
conus_boundary <- st_sf(geometry = conus_boundary, crs = st_crs(conus_states))

# Save unified boundary
boundary_file <- file.path(boundaries_dir, "conus_boundary.gpkg")
st_write(conus_boundary, boundary_file, delete_dsn = TRUE, quiet = TRUE)

cat("CONUS boundary created and saved to:", boundary_file, "\n")

# Create bounding box for processing
conus_bbox <- st_bbox(conus_boundary)
cat("CONUS bounding box:", paste(conus_bbox, collapse = ", "), "\n")

####################################################################################################################
# DOWNLOAD AND PROCESS NLCD DATA
####################################################################################################################

nlcd_files <- list()

for (year in nlcd_years) {
  cat("\nProcessing NLCD", year, "...\n")
  
  # Download NLCD data
  nlcd_file <- get_nlcd_conus(year, nlcd_dir)
  
  if (!file.exists(nlcd_file)) {
    cat("Warning: NLCD file not found for year", year, "\n")
    next
  }
  
  nlcd_files[[as.character(year)]] <- nlcd_file
  
  # Load and inspect NLCD data
  nlcd <- rast(nlcd_file)
  
  cat("NLCD", year, "loaded:\n")
  cat("  Resolution:", res(nlcd), "meters\n")
  cat("  Extent:", as.vector(ext(nlcd)), "\n")
  cat("  CRS:", crs(nlcd, proj = TRUE), "\n")
  
  # Get landcover class frequencies
  lc_freq <- freq(nlcd)
  total_pixels <- sum(lc_freq$count)
  
  # Calculate coverage percentages for key classes
  key_classes <- c(21, 22, 23, 24, 41, 42, 43, 52, 71, 81, 82)  # Urban, forest, grass, crop
  key_coverage <- lc_freq[lc_freq$value %in% key_classes, ]
  
  if (nrow(key_coverage) > 0) {
    key_coverage$percent <- (key_coverage$count / total_pixels) * 100
    
    cat("  Key landcover classes coverage:\n")
    for (i in 1:nrow(key_coverage)) {
      class_name <- case_when(
        key_coverage$value[i] %in% c(41, 42, 43) ~ "Forest",
        key_coverage$value[i] %in% c(52, 71) ~ "Grassland",
        key_coverage$value[i] %in% c(81, 82) ~ "Crop",
        key_coverage$value[i] == 21 ~ "Urban-Open",
        key_coverage$value[i] == 22 ~ "Urban-Low",
        key_coverage$value[i] == 23 ~ "Urban-Medium", 
        key_coverage$value[i] == 24 ~ "Urban-High",
        TRUE ~ paste("Class", key_coverage$value[i])
      )
      
      cat(sprintf("    %s (%d): %.2f%%\n", 
                  class_name, key_coverage$value[i], key_coverage$percent[i]))
    }
  }
}

if (length(nlcd_files) == 0) {
  stop("No NLCD files successfully downloaded")
}

####################################################################################################################
# CREATE LANDCOVER MASKS
####################################################################################################################

# Use the most recent NLCD year for mask creation
latest_year <- max(as.numeric(names(nlcd_files)))
latest_nlcd_file <- nlcd_files[[as.character(latest_year)]]

cat("\nCreating landcover masks using NLCD", latest_year, "...\n")

# Create all landcover masks
landcover_masks <- create_landcover_masks_local(
  nlcd_file = latest_nlcd_file,
  conus_boundary = conus_boundary,
  output_dir = masks_dir
)

# Verify mask creation
cat("Landcover masks created:\n")
for (lc_name in names(landcover_masks)) {
  mask_file <- landcover_masks[[lc_name]]
  
  if (file.exists(mask_file)) {
    mask_raster <- rast(mask_file)
    mask_area_km2 <- sum(values(mask_raster), na.rm = TRUE) * prod(res(mask_raster)) / 1e6
    
    cat(sprintf("  %s: %.0f km² (%.2f%% of CONUS)\n", 
                lc_name, mask_area_km2, (mask_area_km2 / 8080000) * 100))  # Approximate CONUS area
  } else {
    cat("  ", lc_name, ": ERROR - file not created\n")
  }
}

####################################################################################################################
# CREATE REGIONAL PROCESSING DIVISIONS
####################################################################################################################

# Create census region boundaries for efficient processing
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

# Create regional boundary files
regional_boundaries <- list()

for (region_name in names(census_regions)) {
  
  region_states <- census_regions[[region_name]]
  region_boundary <- conus_states[conus_states$NAME %in% region_states, ]
  region_boundary <- st_union(region_boundary)
  region_boundary <- st_sf(region = region_name, geometry = region_boundary)
  
  # Save regional boundary
  region_file <- file.path(boundaries_dir, paste0("region_", tolower(region_name), ".gpkg"))
  st_write(region_boundary, region_file, delete_dsn = TRUE, quiet = TRUE)
  
  regional_boundaries[[region_name]] <- region_file
  
  # Calculate region area
  region_area_km2 <- as.numeric(st_area(region_boundary)) / 1e6
  
  cat("Region", region_name, ":", length(region_states), "states,", 
      round(region_area_km2), "km²\n")
}

####################################################################################################################
# CREATE PROCESSING TILES FOR MEMORY MANAGEMENT
####################################################################################################################

# Create spatial tiles for processing very large rasters
create_processing_tiles <- function(boundary, tile_size = 5) {  # degrees
  
  bbox <- st_bbox(boundary)
  
  # Create grid
  x_seq <- seq(bbox["xmin"], bbox["xmax"], by = tile_size)
  y_seq <- seq(bbox["ymin"], bbox["ymax"], by = tile_size)
  
  tiles <- expand.grid(x = x_seq[-length(x_seq)], y = y_seq[-length(y_seq)])
  
  tile_polygons <- pmap(tiles, function(x, y) {
    st_polygon(list(matrix(c(x, y, x + tile_size, y, x + tile_size, y + tile_size, 
                            x, y + tile_size, x, y), ncol = 2, byrow = TRUE)))
  })
  
  tiles_sf <- st_sf(
    tile_id = 1:length(tile_polygons),
    x_min = tiles$x,
    y_min = tiles$y,
    geometry = st_sfc(tile_polygons, crs = st_crs(boundary))
  )
  
  # Intersect with boundary to keep only relevant tiles
  tiles_intersect <- st_intersects(tiles_sf, boundary, sparse = FALSE)[,1]
  tiles_sf <- tiles_sf[tiles_intersect, ]
  
  return(tiles_sf)
}

# Create processing tiles
processing_tiles <- create_processing_tiles(conus_boundary, tile_size = 5)

cat("Created", nrow(processing_tiles), "processing tiles for memory-efficient operations\n")

# Save processing tiles
tiles_file <- file.path(boundaries_dir, "processing_tiles.gpkg")
st_write(processing_tiles, tiles_file, delete_dsn = TRUE, quiet = TRUE)

####################################################################################################################
# VALIDATE LANDCOVER MASKS
####################################################################################################################

# Function to validate a landcover mask
validate_mask <- function(mask_file, mask_name) {
  
  if (!file.exists(mask_file)) {
    return(list(valid = FALSE, error = "File does not exist"))
  }
  
  tryCatch({
    mask <- rast(mask_file)
    
    # Check basic properties
    values_range <- minmax(mask)
    unique_vals <- unique(values(mask))
    
    # Should be binary mask (0 and 1)
    is_binary <- all(unique_vals %in% c(0, 1, NA))
    
    # Calculate coverage
    total_pixels <- ncell(mask)
    mask_pixels <- sum(values(mask) == 1, na.rm = TRUE)
    coverage_percent <- (mask_pixels / total_pixels) * 100
    
    list(
      valid = TRUE,
      binary = is_binary,
      coverage_percent = coverage_percent,
      total_pixels = total_pixels,
      mask_pixels = mask_pixels,
      resolution = res(mask),
      extent = as.vector(ext(mask))
    )
    
  }, error = function(e) {
    list(valid = FALSE, error = as.character(e))
  })
}

# Validate all masks
cat("\nValidating landcover masks...\n")

mask_validation <- map_dfr(names(landcover_masks), function(lc_name) {
  
  mask_file <- landcover_masks[[lc_name]]
  validation <- validate_mask(mask_file, lc_name)
  
  if (validation$valid) {
    cat(sprintf("  %s: %.2f%% coverage, %s pixels\n", 
                lc_name, validation$coverage_percent, 
                format(validation$mask_pixels, big.mark = ",")))
    
    tibble(
      landcover = lc_name,
      valid = TRUE,
      coverage_percent = validation$coverage_percent,
      mask_pixels = validation$mask_pixels
    )
  } else {
    cat("  ", lc_name, ": ERROR -", validation$error, "\n")
    
    tibble(
      landcover = lc_name,
      valid = FALSE,
      coverage_percent = NA,
      mask_pixels = NA
    )
  }
})

# Save validation results
validation_file <- file.path(masks_dir, "mask_validation.csv")
write_csv(mask_validation, validation_file)

####################################################################################################################
# CREATE PROCESSING SUMMARY
####################################################################################################################

# Create comprehensive processing summary
processing_summary <- list(
  nlcd_years = nlcd_years,
  nlcd_files = nlcd_files,
  landcover_masks = landcover_masks,
  regional_boundaries = regional_boundaries,
  conus_boundary = boundary_file,
  processing_tiles = tiles_file,
  mask_validation = mask_validation
)

# Save processing summary
summary_file <- file.path(data_dir, "landcover_processing_summary.rds")
saveRDS(processing_summary, summary_file)

cat("\nProcessing summary saved to:", summary_file, "\n")

####################################################################################################################
# NEXT STEPS AND RECOMMENDATIONS
####################################################################################################################

cat("
Landcover Processing Complete!

Created Assets:
1. CONUS state boundaries (", nrow(conus_states), " states)
2. Unified CONUS boundary
3. Regional boundaries (4 census regions)
4. Processing tiles (", nrow(processing_tiles), " tiles)
5. Landcover masks (", length(landcover_masks), " types)

Data Locations:
- Boundaries: ", boundaries_dir, "
- NLCD data: ", nlcd_dir, "
- Landcover masks: ", masks_dir, "

Landcover Coverage Summary:
")

valid_masks <- mask_validation[mask_validation$valid, ]
if (nrow(valid_masks) > 0) {
  for (i in 1:nrow(valid_masks)) {
    cat(sprintf("- %s: %.2f%% of CONUS\n", 
                valid_masks$landcover[i], valid_masks$coverage_percent[i]))
  }
}

cat("
Next Steps:
1. Run time series extraction: 04_extract_timeseries_local.R
2. Process satellite data with landcover masks
3. Generate drought indicators and analysis

Processing Notes:
- Use regional boundaries for memory-efficient processing
- Processing tiles available for very large raster operations
- All assets use consistent CRS and spatial alignment
- Validation results saved for quality control

Ready for satellite data processing!
")

cat("Landcover processing completed at:", as.character(Sys.time()), "\n")