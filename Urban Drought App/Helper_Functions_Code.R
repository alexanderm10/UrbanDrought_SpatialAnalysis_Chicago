####################################################################################################################

#Purpose: Include any helper functions & code for Urban_Drought_app.R


####################################################################################################################

library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(lubridate)
library(tidyr)

source("Graph_plotting.R") # some of the work is already done in this file 

NDVI_data <-read_csv("/Users/jocelyngarcia/Documents/GitHub/UrbanDrought_SpatialAnalysis_Chicago/Urban Drought App/drought_monitoring_csvs/raw_data_k=12.csv")%>%
  mutate(date = as.Date(date, format="%Y-%m-%d"))

CI_csv <- read_csv("/Users/jocelyngarcia/Documents/GitHub/UrbanDrought_SpatialAnalysis_Chicago/Urban Drought App/drought_monitoring_csvs/k=12_norms_all_LC_types.csv")


####################################################################################################################
#Gallery functions (not working at the moment 2/17 - but not sure if graphics are needed at all)
LC_part <- c("for", "crop", "grass", "uh", "um", "ul", "uo")

graphic_return <- function(LC_file, LC_part) {
  if (length(LC_file) == 0) {
    return(tags$p("No images found."))
  }
  
  img_list <- lapply(seq_along(LC_file), function(i) {
    column(
      width = 7,  # Adjust column width as needed (12 columns per row)
      imageOutput(paste0(LC_part[i], "_image", i))
    )
  })
  
  return(img_list)
}

graphic_formatting <- function(for_files, type, category, output) {
  output$plot <- renderPlot({
    ggplot(for_files, aes(x = year, y = NDVI)) +
      geom_line() +
      ggtitle(paste("NDVI Trends for", type, category))
  })
}

####################################################################################################################
#Function to determine color of KPI Status Box for each LC Type 
#Purpose: Need to pull mean and most recent NDVI value, find difference, and set box color to reflect it's status 

#putting NDVI_data in order by date
NDVI_data <-NDVI_data[order(as.Date(NDVI_data$date, format="%Y-%m-%d"), decreasing = TRUE), ]

head(NDVI_data)

#Pulling yday of latest data (most recent day) 

#finding latest day & pulling date
latest_day<-head(NDVI_data, 1)
date_needed <-latest_day$date

#pulling any rows with matching date 
most_recent_data<- filter(NDVI_data, date == date_needed)

LC_status <- function(LC_type, NDVI_data, CI_csv, most_recent_data) {
  
  # Ensure consistent use of LC_type in filter() calls
  NDVI_subset <- filter(NDVI_data, type == LC_type)
  CI_subset <- filter(CI_csv, type == LC_type)
  most_recent_subset <- filter(most_recent_data, type == LC_type)
  
  # Check if most_recent_subset has any data
  if (nrow(most_recent_subset) == 0) {
    return(NULL)  # Avoid errors if no data matches
  }
  
  # Extract recent yday
  recent_yday <- most_recent_subset$yday[1]
  CI_final_subset <- filter(CI_subset, yday == recent_yday)
  
  # Ensure CI_final_subset is not empty before extracting mean
  if (nrow(CI_final_subset) == 0) {
    return(NULL)  # Handle missing data
  }
  
  # Extract mean value
  mean_value <- CI_final_subset$mean[1]
  
  # Compute status
  status <- round((most_recent_subset$NDVI - mean_value), digits = 5)
  
  # Determine color based on status
  color <- ifelse(status >= 0, "green",
                  ifelse(status >= -0.01, "yellow",
                         ifelse(status >= -0.02, "orange", "red")))
  
  
  
  return(list(status = status, color = color))
}
