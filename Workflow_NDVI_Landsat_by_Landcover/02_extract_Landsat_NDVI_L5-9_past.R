# Pull all available Landsat NDVI; save by product; do not merge products into same file
# TO BE RUN ONCE!
# Steps for each landsat product:
# 1. Pull in the date for the area and date range we want: Jan 1, 2000 - today; filter/clip to our region
# 2. Do data cleaning: apply the appropriate bitmask, do band-level corrections
# 3. Mosaic images together with a middle-centered 15 day window (image date +/- 7 days); using a median reducer function
#      -->  mosiacking of images within a week on either side of a single image *should* help reduce spatial-based noise in NDVI
#    -- CONSIDER SAVING THESE IMAGES FOR LATER!
# 4. Create landcover-specific collections using the existing nlcd-based masks (see script 01) 
# 5. Reduce to the mean value for each image date
# 6. Save a file for each Landsat product (e.g. Landsat 7, Landsat 8) with the time series for each landcover class -- 1 value per landcover class per date
