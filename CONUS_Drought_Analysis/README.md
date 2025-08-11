# CONUS Drought Analysis using HLSL30 Data

This repository contains R scripts for conducting continental United States (CONUS) scale drought analysis using NASA's HLSL30 (Landsat harmonized surface reflectance) data. This analysis scales up the Chicago regional drought monitoring methodology to cover the entire CONUS region.

## Overview

### Objectives
- Scale urban drought analysis from Chicago region to full CONUS
- Utilize HLSL30 harmonized Landsat data for improved temporal coverage
- Generate NDVI-based drought indicators by landcover type across CONUS
- Create regional and national drought monitoring capabilities

### Dataset: HLSL30
- **Product**: NASA HLS HLSL30 v002 
- **Description**: Landsat harmonized surface reflectance and TOA brightness
- **Spatial Resolution**: 30m
- **Temporal Coverage**: 2013-present
- **Key Advantages**: Harmonized with Sentinel-2, atmospheric correction applied, daily global coverage

## Repository Structure

```
CONUS_Drought_Analysis/
├── 00_Setup_and_Boundaries/          # CONUS boundary and NLCD setup
├── 01_HLSL30_Data_Processing/        # HLSL30 data extraction and NDVI calculation  
├── 02_Landcover_Processing/          # Landcover mask creation
├── 03_Regional_Analysis/             # State/regional analysis scripts
├── 04_Visualization_and_Export/      # Data visualization and export tools
└── Helper_Functions/                 # Shared utility functions
```

## Processing Workflow

### Phase 1: Setup and Boundaries
1. **01_setup_CONUS_boundaries.R**
   - Define CONUS boundary (excludes AK, HI) 
   - Load NLCD data for 2000-2025
   - Create census region divisions
   - Export boundaries to Earth Engine assets

### Phase 2: Landcover Processing
1. **01_create_CONUS_landcover_masks.R**
   - Generate landcover-specific masks for 7 types:
     - Forest (NLCD: 41, 42, 43)
     - Grassland (NLCD: 52, 71)
     - Crop (NLCD: 81, 82)  
     - Urban-Open (NLCD: 21)
     - Urban-Low (NLCD: 22)
     - Urban-Medium (NLCD: 23)
     - Urban-High (NLCD: 24)

### Phase 3: HLSL30 Data Processing
1. **01_extract_HLSL30_NDVI_CONUS.R**
   - Load HLSL30 collection (2013-present)
   - Apply quality masks and cloud filtering
   - Calculate NDVI from B4 (red) and B5 (NIR)
   - Extract regional means by landcover type
   - Export time series data to CSV

### Phase 4: Analysis and Visualization
- Regional trend analysis
- Drought indicator calculation
- Comparative analysis across regions
- Visualization dashboard development

## Key Features

### CONUS-Scale Adaptations
- **Regional Processing**: Divided into 4 census regions (Northeast, Midwest, South, West)
- **Memory Management**: Chunked processing for large-scale computations
- **Quality Control**: HLSL30 Fmask-based filtering
- **Scalable Architecture**: Modular design for regional or national processing

### Processing Considerations
- **Computational Requirements**: Substantial Earth Engine quota usage
- **Processing Time**: Individual tasks may take 2-8 hours
- **Data Volume**: Multi-gigabyte outputs per landcover type
- **Regional Approach**: Recommended for initial processing and validation

### HLSL30 Advantages over Traditional Landsat
- **Higher Temporal Frequency**: Daily revisit through Landsat 8/9 combination
- **Harmonized Data**: Consistent across sensor and mission transitions  
- **Atmospheric Correction**: Pre-processed surface reflectance
- **Quality Assessment**: Comprehensive Fmask cloud/shadow detection

## Getting Started

### Prerequisites
```r
# Required R packages
library(rgee)      # Google Earth Engine interface
library(raster)    # Spatial data processing
library(terra)     # Modern spatial data handling
library(sf)        # Vector spatial data
library(lubridate) # Date manipulation
```

### Earth Engine Setup
1. Install and configure Google Earth Engine account
2. Initialize rgee: `rgee::ee_Initialize(user = 'your-email@domain.com', drive=T)`
3. Ensure adequate Earth Engine processing quotas

### Processing Steps
1. **Run Setup Scripts**: Start with `00_Setup_and_Boundaries/01_setup_CONUS_boundaries.R`
2. **Create Landcover Masks**: Run `02_Landcover_Processing/01_create_CONUS_landcover_masks.R`
3. **Wait for Assets**: Allow Earth Engine tasks to complete (may take several hours)
4. **Extract HLSL30 Data**: Run `01_HLSL30_Data_Processing/01_extract_HLSL30_NDVI_CONUS.R`

### Processing Options
- **Test Mode**: Process single region/landcover combination first
- **Regional Mode**: Process by census regions (recommended)
- **Full CONUS**: Process entire continental US (resource intensive)

## Data Outputs

### CSV Time Series
- **Naming**: `HLSL30_[Region]_[Landcover]_[timestamp].csv`
- **Columns**: date, NDVI
- **Temporal Resolution**: ~weekly (7-day mosaics)
- **Geographic Coverage**: Regional or CONUS-wide means

### Earth Engine Assets
- **Boundaries**: CONUS states boundary
- **Landcover**: Annual NLCD (2000-2025)  
- **Masks**: Landcover-specific masks by type and year

## Processing Recommendations

### Initial Setup
1. Start with test processing (single region/landcover)
2. Validate data quality and processing parameters
3. Scale to regional processing after verification

### Resource Management
- Monitor Earth Engine quotas during large exports
- Use regional processing to manage computational load
- Implement delays between task submissions
- Process during off-peak hours when possible

### Quality Assurance
- Verify landcover mask creation before NDVI extraction
- Sample outputs across different regions for consistency
- Compare results with existing Chicago analysis for validation

## Comparison with Chicago Analysis

### Similarities
- Same landcover classification scheme (NLCD)
- NDVI-based drought indicators
- Time series analysis approach
- R/Earth Engine processing pipeline

### Key Differences
- **Scale**: CONUS vs 7-county region
- **Data Source**: HLSL30 vs individual Landsat missions
- **Processing**: Regional chunking vs single geometry
- **Temporal Coverage**: 2013-present vs 2000-present
- **Computational Requirements**: Substantially higher resource needs

## Future Enhancements

### Analysis Extensions
- State-level drought indicators
- Seasonal trend analysis
- Extreme drought event detection
- Integration with climate data

### Technical Improvements
- Automated processing pipelines  
- Real-time data updates
- Web-based visualization dashboard
- Integration with existing drought monitoring systems

## Troubleshooting

### Common Issues
- **Earth Engine Quotas**: Reduce processing extent or increase timeout values
- **Memory Errors**: Use regional processing approach
- **Asset Dependencies**: Ensure setup scripts complete before data extraction
- **Path Issues**: Verify Google Drive paths match your setup

### Performance Tips
- Use `bestEffort=TRUE` for large region processing
- Set appropriate `maxPixels` parameters (1e10-1e12)
- Monitor task queues to avoid overwhelming Earth Engine
- Implement proper error handling and retry logic

## Contact and Support

This analysis builds upon the Urban Drought monitoring framework developed for the Chicago region. For questions about methodology or implementation, refer to the original Chicago analysis documentation and helper functions.

---

**Note**: This is a computationally intensive analysis requiring substantial Google Earth Engine resources. Plan processing accordingly and monitor quotas during execution.