# Explore temporal trends in NDVI for the 7-county region

# 1. Get MODIS NDVI and Land Cover for the 7-county region
#    1.a. GEE: Clip MODIS NDVI, resample to constant proj of 1st; store/download asset
#    1.b. GEE: Clip LC, resample to constant proj of NDVI; store/download asset


# 2. Indentify what's "Normal" Charatercize annual NDVI trends by LC type
#    NOTE: Not going to run a per-pixel analysis, but rather throw everything in together FTM
#    2.a. Simple Approach: reduce regions to get single mean time series for each LC type; download and play around with
#    2.b. R-based approach: May need to optimize bc will be BIG data
#.        2.1. Merge NDVI & LC data together (this will be BIG; may need to average)
# 

lcPalette <-  c("1-EvgNeedleFor"='#05450a', 
                "2-EvgBroadFor"='#086a10', 
                "3-DecidNeedleFor"='#54a708', 
                "4-DecidBroadFor"='#78d203', 
                "5-MixedForest"='#009900', 
                "6-ClosedShrub"='#c6b044', 
                "7-OpenShrub"='#dcd159',
                "8-WoodySav"='#dade48',
                "9-Sav"='#fbff13', 
                "10-Grassland"='#b6ff05', 
                "11-Wetland"='#27ff87', 
                "12-Crop"='#c24f44', 
                "13-Urban"='#a5a5a5', 
                "14-Mosaic"='#ff6d4c',
                "15-Snow"='#69fff8', 
                "16-Barren"='#f9ffa4', 
                "17-Water"='#1c0dff')