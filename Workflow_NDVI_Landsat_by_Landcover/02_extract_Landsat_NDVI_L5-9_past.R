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

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)
path.google.CR <- "~/Google Drive/My Drive/UrbanEcoDrought/"
path.google.share <- "~/Google Drive/Shared drives/Urban Ecological Drought/"
# GoogleFolderSave <- "UHI_Analysis_Output_Final_v2"
assetHome <- ee_get_assethome()

##################### 
# 0. Read in helper functions ----
##################### 
source("00_EarthEngine_HelperFunctions.R")
##################### 


##################### 
# Color Palette etc. ----
##################### 
# Setting the center point for the Arb because I think we'll have more variation
Map$setCenter(-88.04526, 41.81513, 11);

ndviVis = list(
  min= 0.0,
  max= 1,
  palette= c(
    '#FFFFFF', '#CE7E45', '#DF923D', '#F1B555', '#FCD163', '#99B718', '#74A901',
    '#66A000', '#529400', '#3E8601', '#207401', '#056201', '#004C00', '#023B01',
    '#012E01', '#011D01', '#011301'
    )
  )
##################### 

##################### 
# Read in Landcover Masks ----
##################### 
forMask <- ee$Image('users/crollinson/NLCD-Chicago_2000-2023_Forest')
# ee_print(forMask)
# Map$addLayer(forMask$select("YR2023"))

grassMask <- ee$Image('users/crollinson/NLCD-Chicago_2000-2023_Grass')
# Map$addLayer(grassMask$select("YR2023"))

cropMask <- ee$Image('users/crollinson/NLCD-Chicago_2000-2023_Crop')

urbOMask <- ee$Image('users/crollinson/NLCD-Chicago_2000-2023_Urban-Open')

urbLMask <- ee$Image('users/crollinson/NLCD-Chicago_2000-2023_Urban-Low')

urbMMask <- ee$Image('users/crollinson/NLCD-Chicago_2000-2023_Urban-Medium')

urbHMask <- ee$Image('users/crollinson/NLCD-Chicago_2000-2023_Urban-High')

# Map$addLayer(urbLMask$select("YR2023"))
# Map$addLayer(forMask$select("YR2023"))
##################### 

##################### 
# Read in & Format Landsat 8 ----
##################### 
# "LANDSAT/LC08/C02/T1_RT"
# Load MODIS NDVI data; attach month & year
# https:#developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C02_T1_L2
landsat8 <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$filterBounds(Chicago)$map(function(image){
  return(image$clip(Chicago))
})$map(function(img){
  d= ee$Date(img$get('system:time_start'));
  dy= d$get('day');    
  m= d$get('month');
  y= d$get('year');
  
  # # Add masks 
  img <- applyLandsatBitMask(img)
  
  # #scale correction; doing here & separating form NDVI so it gets saved on the image
  lAdj = img$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7'))$multiply(0.0000275)$add(-0.2);
  lst_k = img$select('ST_B10')$multiply(0.00341802)$add(149);

  # img3 = img2$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y)
  return(img$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y))
})$select(c('SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7', 'ST_B10'),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K'))$map(addNDVI)
# Map$addLayer(landsat8$first()$select('NDVI'), ndviVis, "NDVI - First")
# ee_print(landsat8)
# Map$addLayer(landsat8$first()$select('NDVI'))

l8Mosaic = mosaicByDate(landsat8, 7)$select(c('blue_median', 'green_median', 'red_median', 'nir_median', 'swir1_median', 'swir2_median', 'LST_K_median', "NDVI_median"),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K', "NDVI"))$sort("date")
# ee_print(l8Mosaic, "landsat8-Mosaic")
# Map$addLayer(l8Mosaic$first()$select('NDVI'), ndviVis, "NDVI - First")
##################### 

##################### 
# Mask NDVI by Landcover & condense to regional means ----
##################### 

# yrLC <- 
# yrsLandsatNow <- ee$List(l8Mosaic$aggregate_array("year"))$distinct()
# yrStrLC <- ee$List(paste0("YR", yrsLandsatNow$getInfo()))
bandYrs <- forMask$bandNames()
bandYrs$getInfo()
ee$Number(ee$String(bandYrs$get(0))$slice(2))$getInfo()

# Map$addLayer(l8Mosaic$select("NDVI")$first(), ndviVis, "Landsat8 Mosaic NDVI") # It worked!!
# Map$addLayer(landsat8$select("NDVI")$first(), ndviVis, "Landsat8 Mosaic NDVI") # It worked!!

ndviFor = bandYrs$map(ee_utils_pyfunc(function(strYR){
  YR <- ee$Number$parse(ee$String(strYR)$slice(2));
  START <- ee$Date$fromYMD(YR,1,1);
  END <- ee$Date$fromYMD(YR,12,31);
  
  forYR <- l8Mosaic$filter(START, END)
  ee_print(forYR)
  
}))


# - Low Urban ----
# Extract NDVI --> to redo and loop through the years, you can use the yrLstString to do the mapping and that should work better; look at code from UHI workflow
# ndviUrbLYearList = 

ndviUrbLYear = l8Mosaic$map(function(img){ 
  # Note: This is very slow, but I don't know how else to match the bands
  yrNow = ee$Number(img$get('year'))$format()$slice(0) # May be able to work around the slice, but I kept getting format issues
  yrStr = ee$String("YR")$cat(yrNow) # Need to figure out how to pull the right band
  
  maskNow = urbLMask$select(yrStr); # Very clunky, but it works!
  
  return(img$updateMask(maskNow))
  })
# ee_print(ndviUrbLYear)
# Map$addLayer(ndviUrbLYear$select("NDVI")$first(), ndviVis, "Forest NDVI") # It worked!!

# # projLandcover = nlcdchi.first().projection()
# # print(projLandcover)

######### ************************** ######### 
######### Broken Code!!! ----
######### ************************** ######### 
# reduce regions: THIS ISN"T WORKING YET!!!
UrbLMeans = ee$FeatureCollection(ndviUrbLYear$map(function(img){
  RedMn = img$select("NDVI")$reduceRegion(reducer= ee$Reducer$mean(), geometry=Chicago$geometry(),
    scale=30, # hard-coded, but it's what has to happen to work
    maxPixels=1e13)
  # test <- ee$Feature(NULL, ForMn)$set('system:time_start', img$get('system:time_start'))$set('date', ee$Date(img$get('system:time_start'))$format("YYYY-MM-dd"))
  return(ee$Feature(NULL, RedMn)$set('system:time_start', img$get('system:time_start'))$set('date', ee$Date(img$get('system:time_start'))$format("YYYY-MM-dd")))
}))
ee_print(UrbLMeans, "UrbL Means") # 

# # # # Saving the outputs!
UrbLMeansList = ee$List(UrbLMeans$distinct('Name')$aggregate_array('Name'))$cat(c('time', 'NDVI')) # LIST HERE THE PROPERTIES YOU WANT TO WRITE
# UrbLMeansList$getInfo()
UrbLMeansSave <- ee_table_to_drive(collection=UrbLMeans, 
                                  description="Save_Urb-L-Means_Landsat8",
                                  folder="UrbanEcoDrought_TEST",
                                  fileNamePrefix="Landsat8_Urban-Low",
                                  timePrefix=T,
                                  fileFormat="CSV",
                                  selectors=c("time", "NDVI"))
UrbLMeansSave$start()

test <- read.csv("~/Google Drive/My Drive/UrbanEcoDrought_TEST/Landsat8_Urban-Low_2023_07_11_12_06_34.csv")
head(test)
  # .evaluate(function (selectors) {
  #   # selectors must be a client-side object, so evaluate first.
  #   # If you know them up-front, just plug them in when exporting.
  #   desc1 = 'table_demo_'
  #   Export.table.toDrive({
  #     collection: forMeans,
  #     description: 'Chi-NDVI-Forest-Landsat_8-9_mosaic',
  #     folder: 'UrbanEcoDrought',
  #     fileFormat: 'CSV',
  #     selectors: selectors
  #   })
  # });
######### ************************** ######### 

  


##################### 

