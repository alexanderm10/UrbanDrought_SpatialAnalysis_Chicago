# Local Processing Pipeline for CONUS Drought Analysis

This directory contains a complete alternative implementation for CONUS-scale drought analysis that **does not require Google Earth Engine**. This approach uses direct data access, local processing, and open-source tools.

## Overview

### Why Local Processing?
- **Independence**: No reliance on GEE quotas or changing terms
- **Control**: Full control over processing parameters and algorithms  
- **Scalability**: Can leverage local HPC resources or cloud computing
- **Reproducibility**: Processing environment under full user control
- **Cost**: Potentially lower costs for large-scale processing

### Key Differences from GEE Approach
| Aspect | GEE Version | Local Version |
|--------|-------------|---------------|
| **Data Access** | GEE Catalog | Direct NASA/USGS APIs |
| **Processing** | GEE Servers | Local/Cloud Computing |
| **Storage** | GEE Assets | Local Files/Cloud Storage |
| **Scaling** | Automatic | Manual Parallelization |
| **Costs** | GEE Quotas | Compute/Storage Costs |

## Directory Structure

```
Alternative_Methods/
├── Helper_Functions/
│   └── local_processing_functions.R     # Core processing functions
├── 02_Data_Acquisition/
│   └── 01_acquire_hls_data.R            # Download HLS/Landsat data
├── 03_Local_Processing/
│   └── 01_process_landcover_local.R     # Create landcover masks
├── 04_Time_Series_Analysis/
│   └── 01_extract_timeseries_local.R    # Extract NDVI time series
└── README_Local_Processing.md           # This file
```

## Data Sources

### Satellite Data
1. **NASA HLS (Preferred)**
   - Direct access via NASA Earthdata
   - Harmonized Landsat/Sentinel-2 data  
   - 30m resolution, daily coverage
   - Pre-processed surface reflectance

2. **Microsoft Planetary Computer**
   - Free tier available
   - Python/R interfaces
   - Landsat, Sentinel-2, MODIS

3. **AWS Open Data**
   - Landsat on AWS
   - Sentinel-2 on AWS
   - No egress charges for analysis

4. **USGS Earth Explorer**
   - Direct Landsat downloads
   - Manual or API access
   - Collection 2 data

### Ancillary Data
- **NLCD**: Multi-Resolution Land Characteristics Consortium
- **Boundaries**: US Census Bureau TIGER/Line
- **MODIS**: NASA LAADS DAAC (alternative to Landsat)

## Processing Workflow

### Phase 1: Setup and Data Acquisition
1. **Configure Authentication**
   - NASA Earthdata credentials
   - Cloud platform authentication (if using)
   
2. **Download Base Data**
   - NLCD landcover data
   - CONUS state boundaries
   - Create regional divisions

3. **Acquire Satellite Data**
   - Search HLS/Landsat catalogs
   - Download raw data files
   - Process to surface reflectance (if needed)

### Phase 2: Local Processing
1. **Create Landcover Masks**
   - Process NLCD to binary masks
   - Generate masks for 7 landcover types
   - Align with satellite data grid

2. **Process Satellite Data**
   - Calculate NDVI from red/NIR bands
   - Apply quality masks
   - Create temporal composites

3. **Extract Time Series**
   - Apply landcover masks to NDVI
   - Calculate regional means
   - Generate CSV time series

### Phase 3: Analysis and Visualization
1. **Drought Indicators**
   - Calculate anomalies and percentiles
   - Generate drought classifications
   - Compare with reference periods

2. **Visualization**
   - Create maps and time series plots
   - Regional comparison dashboards
   - Export for web applications

## Technical Requirements

### R Packages
```r
# Core spatial processing
library(terra)      # Modern raster processing
library(sf)         # Vector data handling
library(stars)      # Spatiotemporal arrays

# Data acquisition  
library(httr)       # HTTP requests for APIs
library(jsonlite)   # JSON parsing

# Parallel processing
library(future)     # Async/parallel processing
library(furrr)      # Parallel purrr
library(doParallel) # Parallel backend

# Time series analysis
library(lubridate)  # Date handling
library(dplyr)      # Data manipulation

# Optional cloud interfaces
library(reticulate) # Python integration
library(aws.s3)     # AWS access
```

### System Requirements
- **RAM**: 16GB+ recommended (32GB+ for full CONUS)
- **Storage**: 1TB+ for full dataset
- **CPU**: Multi-core processor for parallel processing
- **Network**: High-speed internet for data downloads

### Authentication Setup
```r
# NASA Earthdata (required for HLS)
setup_earthdata_auth("username", "password")

# Optional: Cloud platforms
Sys.setenv(AWS_ACCESS_KEY_ID = "your_key")
Sys.setenv(AWS_SECRET_ACCESS_KEY = "your_secret")
```

## Getting Started

### 1. Initial Setup
```r
# Load helper functions
source("Helper_Functions/local_processing_functions.R")

# Setup parallel processing
setup_parallel(n_cores = 4)

# Configure data directories
data_dir <- "data/"
```

### 2. Acquire Base Data
```r
# Run landcover processing
source("03_Local_Processing/01_process_landcover_local.R")
```

### 3. Download Satellite Data
```r
# Run HLS data acquisition
source("02_Data_Acquisition/01_acquire_hls_data.R")
```

### 4. Extract Time Series
```r
# Run time series extraction
source("04_Time_Series_Analysis/01_extract_timeseries_local.R")
```

## Processing Strategies

### Regional Processing (Recommended)
- Process by census regions (Northeast, Midwest, South, West)
- Reduces memory requirements
- Enables parallel regional processing
- Easier error recovery

### Tiled Processing
- Divide CONUS into spatial tiles
- Process tiles independently
- Suitable for very large datasets
- Good for distributed computing

### Temporal Chunking
- Process data by time periods (years/seasons)
- Reduces simultaneous data loading
- Good for long time series
- Enables incremental updates

## Performance Optimization

### Memory Management
```r
# Use appropriate data types
ndvi <- rast("file.tif", lyrs = 1)  # Single layer only
values(ndvi) <- as.integer(values(ndvi) * 10000)  # Scale to integer

# Process in chunks
process_in_chunks <- function(raster, fun, chunk_size = 1e6) {
  # Implementation for chunk processing
}
```

### Parallel Processing
```r
# Setup parallel backend
plan(multisession, workers = parallel::detectCores() - 1)

# Process files in parallel
results <- future_map(file_list, process_function, .progress = TRUE)
```

### Storage Optimization
```r
# Use compressed formats
writeRaster(ndvi, "output.tif", 
           datatype = "INT2S",    # 16-bit integer
           compression = "LZW",   # Compression
           overwrite = TRUE)

# Cloud Optimized GeoTIFF (COG)
writeRaster(ndvi, "output.tif", 
           options = c("TILED=YES", "COMPRESS=LZW", "OVERVIEW_RESAMPLING=BILINEAR"))
```

## Data Management

### File Organization
```
data/
├── boundaries/          # State/regional boundaries
├── nlcd/               # NLCD raw data
├── landcover_masks/    # Processed landcover masks
├── hls_raw/            # Raw HLS/satellite data
├── hls_processed/      # Processed NDVI files
├── timeseries/         # Extracted time series
└── results/            # Final analysis outputs
```

### Metadata Tracking
```r
# Create data catalog
catalog <- tibble(
  file_path = file_paths,
  date = extraction_dates,
  landcover_type = lc_types,
  region = regions,
  processing_date = Sys.time()
)

write_csv(catalog, "data_catalog.csv")
```

## Cloud Computing Options

### AWS
- EC2 instances for processing
- S3 for data storage  
- Landsat/Sentinel-2 on AWS
- Batch processing with AWS Batch

### Google Cloud
- Compute Engine for processing
- Cloud Storage for data
- BigQuery for large datasets
- AI Platform for ML workflows

### Microsoft Azure
- Virtual Machines for processing
- Blob Storage for data
- Planetary Computer access
- Machine Learning services

### HPC Clusters
- Slurm/PBS job scheduling
- Shared storage systems
- MPI for distributed processing
- Container-based workflows

## Comparison with GEE Version

### Advantages of Local Processing
✅ **No quotas or usage limits**  
✅ **Full control over algorithms**  
✅ **Use latest software versions**  
✅ **Custom quality control**  
✅ **Data stays local (security)**  
✅ **Can leverage existing HPC**  

### Advantages of GEE Version  
✅ **No infrastructure management**  
✅ **Automatic scaling**  
✅ **Pre-processed datasets**  
✅ **Built-in visualization**  
✅ **Lower setup complexity**  
✅ **Consistent processing environment**  

## Cost Considerations

### Local Processing Costs
- **Hardware**: Initial investment in computing/storage
- **Software**: R packages (mostly free)
- **Data Transfer**: Download costs from NASA/cloud providers
- **Personnel**: Setup and maintenance time

### Cloud Processing Costs
- **Compute**: $0.10-$1.00+ per core-hour
- **Storage**: $0.02-$0.10 per GB-month
- **Data Transfer**: Often free for analysis
- **Overall**: Can be cost-effective for short-term projects

## Troubleshooting

### Common Issues
1. **Memory Errors**
   - Use chunked processing
   - Reduce spatial resolution for testing
   - Process regions separately

2. **Download Failures**  
   - Check authentication credentials
   - Implement retry logic
   - Use stable internet connection

3. **Processing Errors**
   - Validate input data formats
   - Check coordinate reference systems
   - Monitor disk space

4. **Performance Issues**
   - Optimize parallel processing
   - Use appropriate data types
   - Implement caching for repeated operations

### Performance Benchmarks
- **HLS Processing**: ~100-500 files per hour (depending on hardware)
- **NDVI Calculation**: ~1-10 seconds per scene
- **Landcover Masking**: ~10-60 seconds per scene
- **Time Series Extraction**: ~1-5 minutes per landcover type per region

## Future Enhancements

### Workflow Automation
- Automated data discovery and download
- Continuous integration pipelines
- Scheduled processing jobs
- Error notification systems

### Advanced Analytics
- Machine learning integration
- Anomaly detection algorithms
- Seasonal decomposition
- Trend analysis

### Visualization
- Interactive web dashboards
- Real-time data streaming
- Mobile applications
- API development for data access

## Support and Resources

### Documentation
- NASA Earthdata: https://earthdata.nasa.gov/
- Microsoft Planetary Computer: https://planetarycomputer.microsoft.com/
- AWS Open Data: https://aws.amazon.com/opendata/
- R Spatial: https://r-spatial.org/

### Community
- R-sig-geo mailing list
- Stack Overflow (R + remote sensing tags)
- GitHub discussions on spatial packages
- CRAN task views for spatial analysis

---

This local processing approach provides a robust alternative to Google Earth Engine while maintaining the same analytical capabilities for CONUS-scale drought monitoring. Choose the approach that best fits your computational resources, technical requirements, and project timeline.