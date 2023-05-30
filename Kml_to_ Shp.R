library(rgdal)
library(sf)
# read the .kml file
path.google <- "~/Library/CloudStorage/GoogleDrive-breidy@mortonarb.org/Shared drives/Urban Ecological Drought/Neighborhood remote sensing analysis/Data"
kml_data <- st_read(file.path(path.google, "Monitoring Locations.kml"))

# remove Z and M dimensions if present
kml_data <- st_zm(kml_data, drop = TRUE)

# specify the file path where the .shp file will be saved
output_file <- file.path(path.google, "Monitoring Locations.shp")

# write the .shp file
write_options <- c("ARCPY_FIELD_NAME_ENCODING=UTF8")
st_write(kml_data, output_file, write_options = write_options)
