####################################################################################################################

#Purpose: Include any helper functions & code for Urban_Drought_app.R


####################################################################################################################

library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(lubridate)
library(tidyr)


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
