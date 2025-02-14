#Housing for any outdated code incase we decide to use it 

####################################################################################################################
#CI graphs from Shiny App code 
#Reason it's here: replaced with density plots, might be more insightful than CI's at the moment
#UI
tabBox(
  tabPanel(
    "CI for All LC Types",
    plotOutput("all_LC_CI_graph")
  ),
  tabPanel(
    "CI for Selected LC Types",
    plotOutput("selected_LC_CI_graph"),
    checkboxGroupInput(
      inputId = "LC_type",
      label = "Select Landcover Types",
      choices = c(
        "Crop" = "crop",
        "Forest" = "forest",
        "Grassland" = "grassland",
        "Urban High" = "urban-high",
        "Urban Medium" = "urban-medium",
        "Urban Low" = "urban-low",
        "Urban Open" = "urban-open"
      ),
      selected = c("crop", "forest", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open"),
      inline = TRUE
    )
  )
)
#server code 
output$selected_LC_CI_graph <- renderPlot({
  req(input$LC_type)  # Ensure at least one LC type is selected
  selected_LC_CI_graph(input$LC_type)  # Pass selected LC types to the function
})
####################################################################################################################
#Below does the same as the density_plot function, keeping code here in case we need to troubleshoot updates are working correctly 

LC_naming<- c("forest", "crop", "grassland", "uh", "um", "ul", "uo")
LC_types<- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

#Seperating NDVI data based on LC type
crop_NDVI <- filter(NDVI_data, type == "crop")
forest_NDVI <- filter(NDVI_data, type == "forest")
grassland_NDVI <- filter(NDVI_data, type == "grassland")
uh_NDVI <- filter(NDVI_data, type == "urban-high")
um_NDVI <- filter(NDVI_data, type == "urban-medium")
ul_NDVI <- filter(NDVI_data, type == "urban-low")
uo_NDVI <- filter(NDVI_data, type == "urban-open")

#putting NDVI_data in order by date
NDVI_data <-NDVI_data[order(as.Date(NDVI_data$date, format="%Y-%m-%d"), decreasing = TRUE), ]

head(NDVI_data)

#Pulling yday of latest data (most recent day) 

#finding latest day & pulling date
latest_day<-head(NDVI_data, 1)
date_needed <-latest_day$date

#pulling any rows with matching date 
most_recent_data<- filter(NDVI_data, date == date_needed)

crop_most_recent <- filter(most_recent_data, type == "crop")
forest_most_recent <-filter(most_recent_data, type == "forest")
grassland_most_recent <-filter(most_recent_data, type == "grassland")
uh_most_recent <-filter(most_recent_data, type == "urban-high")
um_most_recent <-filter(most_recent_data, type == "urban-medium")
ul_most_recent <-filter(most_recent_data, type == "urban-low")
uo_most_recent <-filter(most_recent_data, type == "urban-open")


#Pulling corresponding upper/lower bound and mean for LC types
crop_CI_info <- filter(CI_csv, type == "crop")
forest_CI_info <- filter(CI_csv, type == "forest")
grassland_CI_info <- filter(CI_csv, type == "grassland")
uh_CI_info <- filter(CI_csv, type == "urban-high")
um_CI_info <- filter(CI_csv, type == "urban-medium")
ul_CI_info <- filter(CI_csv, type == "urban-low")
uo_CI_info <- filter(CI_csv, type == "urban-open")


#Pulling only data from matching yday
c <-filter(crop_CI_info, yday == most_recent_data$yday)
f<-filter(forest_CI_info, yday == most_recent_data$yday)
g<-filter(grassland_CI_info, yday == most_recent_data$yday)
uh<-filter(uh_CI_info, yday == most_recent_data$yday)
um<-filter(um_CI_info, yday == most_recent_data$yday)
ul<-filter(ul_CI_info, yday == most_recent_data$yday)
uo<-filter(uo_CI_info, yday == most_recent_data$yday)


#Density plot for each LC type

#Need this to make the legend
legend_data <- data.frame(
  Type = c("Lower Bound", "Upper Bound", "Mean", "Current NDVI"),
  x = c(NA, NA, NA, NA),  
  y = c(NA, NA, NA, NA)  
)

crop_density <- ggplot(crop_NDVI, aes(x = NDVI)) + 
  geom_density(fill = "#c2a5cf", alpha = 0.5) +  
  
  # Add the bounds as dashed lines with legend
  geom_vline(aes(xintercept = c$lwr, linetype = "Lower Bound"), color = "#40004b", size = 1) +
  geom_vline(aes(xintercept = c$upr, linetype = "Upper Bound"), color = "#40004b", size = 1) +
  
  # Mean point
  geom_point(aes(x = c$mean, y = 0, shape = "Mean"), color = "#40004b", size = 4) +
  
  # Current NDVI point (diamond)
  geom_point(aes(x = crop_most_recent$NDVI, y = 0, shape = "Current NDVI"), fill = "#1b7837", color = "#1b7837", size = 4) +
  
  # Labels
  labs(
    x = "Crop Density Plot",
    y = "Density",
    linetype = "Bound Type",  # Legend title for the lines
    shape = "Point Type"      # Legend title for the points
  ) +
  
  # Manual legend adjustments
  scale_linetype_manual(values = c("Lower Bound" = "dashed", "Upper Bound" = "dashed")) +
  scale_shape_manual(values = c("Mean" = 16, "Current NDVI" = 23)) +
  
  theme_minimal()

forest_density <- ggplot(forest_NDVI, aes(x = NDVI)) + 
  geom_density(fill = "#c2a5cf", alpha = 0.5) + 
  geom_vline(xintercept = f$lwr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_vline(xintercept = f$upr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_point(aes(x = f$mean, y = 0), color = "#40004b", size = 4) +
  geom_point(shape = 23, fill = "#1b7837", aes( x = forest_most_recent$NDVI, y = 0), color = "#1b7837", size = 4) +
  labs(
    x= "Forest Density Plot",
    y= "Density"
  )+
  theme_minimal()

grassland_density <-ggplot(grassland_NDVI, aes(x = NDVI)) + 
  geom_density(fill = "#c2a5cf", alpha = 0.5) + 
  geom_vline(xintercept = g$lwr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_vline(xintercept = g$upr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_point(aes(x = g$mean, y = 0), color = "#40004b", size = 4) +
  geom_point(shape = 23,fill = "#1b7837", aes( x = grassland_most_recent$NDVI, y = 0), color = "#1b7837", size = 4) +
  labs(
    x= "Grassland Density Plot",
    y= "Density"
  )+
  theme_minimal()

uh_density <-ggplot(uh_NDVI, aes(x = NDVI)) + 
  geom_density(fill = "#c2a5cf", alpha = 0.5) + 
  geom_vline(xintercept = uh$lwr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_vline(xintercept = uh$upr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_point(aes(x = uh$mean, y = 0), color = "#40004b", size = 4) +
  geom_point(shape = 23, fill = "#1b7837", aes( x = uh_most_recent$NDVI, y = 0), color = "#1b7837", size = 4) +
  labs(
    x= "Urban-High Density Plot",
    y= "Density"
  )+
  theme_minimal()

um_density <-ggplot(um_NDVI, aes(x = NDVI)) + 
  geom_density(fill = "#c2a5cf", alpha = 0.5) + 
  geom_vline(xintercept = um$lwr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_vline(xintercept = um$upr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_point(aes(x = um$mean, y = 0), color = "#40004b", size = 4) +
  geom_point(shape = 23, fill = "#1b7837", aes( x = um_most_recent$NDVI, y = 0), color = "#1b7837", size = 4) +
  labs(
    x= "Urban-Medium Density Plot",
    y= "Density"
  )+
  theme_minimal()

ul_density <-ggplot(ul_NDVI, aes(x = NDVI)) + 
  geom_density(fill = "#c2a5cf", alpha = 0.5) + 
  geom_vline(xintercept = ul$lwr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_vline(xintercept = ul$upr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_point(aes(x = ul$mean, y = 0), color = "#40004b", size = 4) +
  geom_point(shape = 23,fill = "#1b7837", aes( x = ul_most_recent$NDVI, y = 0), color = "#1b7837", size = 4) +
  labs(
    x= "Urban-Low Density Plot",
    y= "Density"
  )+
  theme_minimal()

uo_density <-ggplot(uo_NDVI, aes(x = NDVI)) + 
  geom_density(fill = "#c2a5cf", alpha = 0.5) + 
  geom_vline(xintercept = uo$lwr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_vline(xintercept = uo$upr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_point(aes(x = uo$mean, y = 0), color = "#40004b", size = 4) +
  geom_point(shape = 23,fill = "#1b7837", aes( x = uo_most_recent$NDVI, y = 0), color = "#1b7837", size = 4) +
  labs(
    x= "Urban-Open Density",
    y= "Density"
  )+
  theme_minimal()

#Testing the plots
crop_density 
forest_density 
grassland_density
uh_density 
um_density 
ul_density 
uo_density

#Getting difference between mean and current status (current value - mean), if current value is lower than mean it'll be negative
c_status <- round((crop_most_recent$NDVI - c$mean), digits = 5)
f_status <- round((forest_most_recent$NDVI - f$mean), digits = 5)
g_status <- round((grassland_most_recent$NDVI - g$mean), digits = 5)
uh_status <- round((uh_most_recent$NDVI - uh$mean), digits = 5)
um_status <- round((um_most_recent$NDVI - um$mean), digits = 5)
ul_status <- round((ul_most_recent$NDVI- ul$mean), digits = 5)
uo_status <- round((uo_most_recent$NDVI - uo$mean), digits = 5)

########################################################################################################################