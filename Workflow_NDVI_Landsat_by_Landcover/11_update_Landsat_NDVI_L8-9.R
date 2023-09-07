# Update NDVI records for Landsat 8 & 9 with new observations
# To be run ~ weekly!

# Steps:
# 1. Read in the existing Landsat 8 & 9 files to determine what the last observations were
# 2. Set filterDate using last date in existing records minus 7 (see next step)
# -- note: begins same workflow for step 2 --
# 3. Pull in the date for the area and date range we want: Date from step 2 through today; filter/clip to our region
# 4. Do data cleaning: apply the appropriate bitmask, do band-level corrections
# 5. Mosaic images together with a middle-centered 15 day window (image date +/- 7 days); using a median reducer function
#      -->  mosiacking of images within a week on either side of a single image *should* help reduce spatial-based noise in NDVI
#    --> NOTE: do not worry about saving image updates
# 6. Create landcover-specific collections using the existing nlcd-based masks (see script 01) 
# 7. Reduce to the mean value for each image date
# 6. **APPEND** data to the existing file for each Landsat product (e.g. Landsat 8, Landsat 9) with the time series for each landcover class -- 1 value per landcover class per date


