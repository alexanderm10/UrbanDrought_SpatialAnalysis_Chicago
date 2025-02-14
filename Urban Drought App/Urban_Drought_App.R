####################################################################################################################

#Purpose: Be a visualization portal for the Urban Drought Project, meant to be a real time portal that is updated regularly
#Pulls from NDVI_drought_monitoring workflow & UrbanDrought_SpatialAnalysis_Chicago workflow

####################################################################################################################

library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(sf)
library(tidyverse)
library(ggplot2)
library(DT)
library(lubridate)

#For documentation of this app
#https://docs.google.com/document/d/1I8WkmUjuPLf0SS_IF0F6P97xyH3aQhth8m9iYUQM4hs/edit?usp=sharing

source("Graph_Plotting.R")
source("Helper_Functions_Code.R")


#Files generated from Juliana's workflow - will need these to autoupdate
NDVI_data <-read_csv("/Users/jocelyngarcia/Documents/GitHub/UrbanDrought_SpatialAnalysis_Chicago/Urban Drought App/drought_monitoring_csvs/raw_data_k=12.csv")%>%
  mutate(date = as.Date(date, format="%Y-%m-%d"))
CI_csv <- read_csv("/Users/jocelyngarcia/Documents/GitHub/UrbanDrought_SpatialAnalysis_Chicago/Urban Drought App/drought_monitoring_csvs/k=12_norms_all_LC_types.csv")

#putting NDVI_data in order by date
NDVI_data <-NDVI_data[order(as.Date(NDVI_data$date, format="%Y-%m-%d"), decreasing = TRUE), ]

head(NDVI_data)


#Pulling yday of latest data (most recent day) 

#finding latest day & pulling date
latest_day<-head(NDVI_data, 1)
date_needed <-latest_day$date

#pulling any rows with matching date 
most_recent_data<- filter(NDVI_data, date == date_needed)


#Need to run this code before app
lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

panel_plot_files <- list.files(
  path = "/Users/jocelyngarcia/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org/Shared drives/Urban Ecological Drought/data/NDVI_drought_monitoring/figures/04_panel_plots_usdm_deviation_meanNDVI",
  pattern = "\\.png$", 
  full.names = TRUE
)

scatterplot_files <- list.files(
  path = "/Users/jocelyngarcia/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org/Shared drives/Urban Ecological Drought/data/NDVI_drought_monitoring/figures/06_scatterplots_usdm_deviation_growing_season",
  pattern = "\\.png$", 
  full.names = TRUE
)

for_files <- c()
crop_files <- c()
grass_files <- c()
uh_files <- c()
um_files <- c()
ul_files <- c()
uo_files <- c()


for (file in panel_plot_files) {
  if (grepl("forest", file)) {
    for_files <- append(for_files, file)
  } else if (grepl("crop", file)) {
    crop_files <- append(crop_files, file)
  } else if (grepl("grassland", file)) {
    grass_files <- append(grass_files, file)
  } else if (grepl("urban-high", file)) {
    uh_files <- append(uh_files, file)
  } else if (grepl("urban-medium", file)) {
    um_files <- append(um_files, file)
  } else if (grepl("urban-low", file)) {
    ul_files <- append(ul_files, file)
  } else if (grepl("urban-open", file)) {
    uo_files <- append(uo_files, file)
  }
}

for (file in scatterplot_files) {
  if (grepl("forest", file)) {
    for_files <- append(for_files, file)
  } else if (grepl("crop", file)) {
    crop_files <- append(crop_files, file)
  } else if (grepl("grassland", file)) {
    grass_files <- append(grass_files, file)
  } else if (grepl("urban-high", file)) {
    uh_files <- append(uh_files, file)
  } else if (grepl("urban-medium", file)) {
    um_files <- append(um_files, file)
  } else if (grepl("urban-low", file)) {
    ul_files <- append(ul_files, file)
  } else if (grepl("urban-open", file)) {
    uo_files <- append(uo_files, file)
  }
}

img_list<- c()

#from https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html
counties <- sf::read_sf("/Users/jocelyngarcia/Documents/GitHub/UrbanDrought_SpatialAnalysis_Chicago/Urban Drought App/cb_2023_us_county_500k",
                        layer = "cb_2023_us_county_500k")%>% 
  st_transform(crs = 4326)

il_counties <- subset(counties, counties$NAME %in% c(
  "Cook","DuPage","Kane","McHenry","Lake","Will","Kendall") &
    STATE_NAME == "Illinois")


####################################################################################################################

ui <- dashboardPage(skin = "black",
                    dashboardHeader(
                      title = "Urban Drought Dashboard",
                      titleWidth = 300),
                    dashboardSidebar(
                      width = 300,
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                        menuItem("Analysis", tabName = "analysis", icon = icon("gears"),
                                 menuSubItem("NDVI Data Review",
                                             tabName = "NDVI_data_review")),
                        menuItem("Additional Graphics By LC Types", tabName = "graphics", icon = icon("chart-simple"),
                          menuSubItem("Forest Graphics",
                                    tabName = "for_graphics"),
                          menuSubItem("Crop Graphics",
                                      tabName = "crop_graphics"),
                          menuSubItem("Grassland Graphics",
                                      tabName = "grass_graphics"),
                          menuSubItem("Urban-High Graphics",
                                      tabName = "uh_graphics"),
                          menuSubItem("Urban-Medium Graphics",
                                      tabName = "um_graphics"),
                          menuSubItem("Urban-Low Graphics",
                                      tabName = "ul_graphics"),
                          menuSubItem("Urban-Open Graphics",
                                      tabName = "uo_graphics")),
                        menuItem("Specifics", tabName = "specifics", icon = icon("bookmark"))
                      )
                    ),
                    dashboardBody(
                      # Custom CSS for grid layout and positioning
                      tags$head(
                        tags$style(HTML("
   .county-container {
        display: flex;
        justify-content: space-between;
        margin-bottom: 10px;
      }
      .county-box {
        flex: 1;
        text-align: center;
        font-size: 14px;
        font-weight: bold;
        padding: 10px;
        border-radius: 5px;
        margin: 5px;
        color: white;
      }
      .above-normal { background-color: #2E86C1; }
      .below-low { background-color: #F1C40F; }
      .below-medium { background-color: #E67E22; }
      .below-high { background-color: #D35400; }
"))
                        
                      ),
                      
                      tabItems(
                              tabItem(tabName = "dashboard",
                                # Row for counties' drought status
                                fluidRow(
                                  div(class = "county-container",
                                      div(class = "county-box above-normal", "Crop: Above Normal"),
                                      div(class = "county-box above-normal", "Forest: Above Normal"),
                                      div(class = "county-box below-low", "Grass: Below Normal - Low"),
                                      div(class = "county-box below-high", "Urban-High: Below Normal - High"),
                                      div(class = "county-box below-low", "Urban-Medium: Below Normal - Low"),
                                      div(class = "county-box below-medium", "Urban-Low: Below Normal - Medium"),
                                      div(class = "county-box above-normal", "Urban-Open: Above Normal")
                                  )
                                ),
                                
                                # Map layout
                                fluidRow(
                                  column(width = 4, 
                                         leafletOutput("il_county_map", height = "475px")),
                                  #Density plot graphs, put here for formatting
                                  tabBox(
                                    width = 8,
                                    tabPanel(
                                      "Crop Density Plot",
                                      plotOutput("crop_density_plot")
                                    ),
                                    tabPanel(
                                      "Forest Density Plot",
                                      plotOutput("forest_density_plot")
                                    ),
                                    tabPanel(
                                      "Grassland Density Plot",
                                      plotOutput("grassland_density_plot")
                                    ),
                                    tabPanel(
                                      "Urban-High Density Plot",
                                      plotOutput("uh_density_plot")
                                    ),
                                    tabPanel(
                                      "Urban-Medium Density Plot",
                                      plotOutput("um_density_plot")
                                    ),
                                    tabPanel(
                                      "Urban-Low Density Plot",
                                      plotOutput("ul_density_plot")
                                    ),
                                    tabPanel(
                                      "Urban-Open Density Plot",
                                      plotOutput("uo_density_plot")
                                    )
                                  )
                                ),
                                
                                # NDVI Graphs
                                tabBox(
                                  title = "NDVI Data",
                                  id = "tab1",
                                  height = "250px",
                                  width = 12, 
                                  checkboxGroupInput(
                                    inputId = "landcover_types",
                                    label = "Select Landcover Types",
                                    choices = c(
                                      "crop" = "Crop",
                                      "forest" = "Forest",
                                      "grass" = "Grass",
                                      "urban-high" = "Urban High",
                                      "urban-medium" = "Urban Medium",
                                      "urban-low" = "Urban Low",
                                      "urban-open" = "Urban Open"
                                    ),
                                    selected = c("crop", "forest", "grass", "urban-high", "urban-medium", "urban-low", "urban-open"),
                                    inline = TRUE
                                  ),
                                  tabPanel(
                                    "Full Review",
                                    plotOutput("all_data_graph")
                                  ),
                                  tabPanel("Yearly",
                                           plotOutput("yearly_graph"),
                                           dateInput(inputId = "start_date", label = "Enter Start Date", value = as.Date(Sys.Date()) - 365)),
                                  tabPanel("Monthly", 
                                           plotOutput("monthly_graph"),
                                           dateInput(inputId = "mstart_date", label = "Enter Start Date", value = Sys.Date() %m-% months(1))),
                                  tabPanel("Weekly", 
                                           plotOutput("weekly_graph"),
                                           dateInput(inputId = "wstart_date", label = "Enter Start Date", value = as.Date(Sys.Date()) - 7)
                                )
                                
                                )),
                                
                        tabItem(tabName = "NDVI Data Review",

                                div(class = "top-right-slider",
                                    sliderInput("integer", "Years:",
                                                min = 2014, max = 2025,
                                                value = 2020, width = "1050%")),
                                fluidRow(column(width = 12, DT::dataTableOutput("data_table")))
                                
                          ),
                        
                        tabItem(tabName = "for_graphics",
                                fluidRow(
                                  uiOutput("for_gallery") 
                                )),
                        tabItem(tabName = "crop_graphics",
                                fluidRow(
                                  uiOutput("crop_gallery") 
                                )),
                        tabItem(tabName = "grass_graphics",
                                fluidRow(
                                  uiOutput("grass_gallery") 
                                )),
                        tabItem(tabName = "uh_graphics",
                                fluidRow(
                                  uiOutput("uh_gallery") 
                                )),
                        tabItem(tabName = "um_graphics",
                                fluidRow(
                                  uiOutput("um_gallery") 
                                )),
                        tabItem(tabName = "ul_graphics",
                                fluidRow(
                                  uiOutput("ul_gallery") 
                                )),
                        tabItem(tabName = "uo_graphics",
                                fluidRow(
                                  uiOutput("uo_gallery") 
                                )),
                        tabItem(tabName = "Specifics",
                                textInput("txt", "Enter the text to display below:"),
                                textOutput("text")
                        )
                        
                      
                    )
))

# Define server logic
server <- function(input, output, session) {
  
  # Render the map
  output$il_county_map <- renderLeaflet({
    # Initialize the map
    map <- leaflet() %>%
      addTiles() %>%  # Blank map background
      setView(lng = -88, lat = 41.8, zoom = 8)  # Center the map on Illinois
    
    # Loop to add each county as a toggleable layer
    for (county_name in unique(il_counties$NAME)) {
      county_data <- il_counties[il_counties$NAME == county_name, ]
      
      # Add individual county polygons
      map <- map %>%
        addPolygons(
          data = county_data,
          color = "#444444",
          weight = 1,
          opacity = 1,
          fillOpacity = 0.6,
          fillColor = "#FFEDA0",
          label = ~NAME,
          group = county_name
        )
    }
    # Add layer control for individual counties
    map %>%
      addLayersControl(
        overlayGroups = unique(il_counties$NAME),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  #NDVI graphs
  #All Data
  output$all_data_graph <- renderPlot({
      all_data_graph()  
  })
  #Yearly Data
  output$yearly_graph <- renderPlot({
    req(input$start_date) 
    plot <- twelve_month_graph(input$start_date)
    if (!is.null(plot)) {
      return(plot)
    } else {
      return(NULL)
    }
  })
  
  #Monthly Data
  output$monthly_graph <- renderPlot({
    req(input$mstart_date) 
    plot <- monthly_graph(input$mstart_date)
    if (!is.null(plot)) {
      return(plot)
    } else {
      return(NULL)
    }
  })
  
  #Weekly Data
  output$weekly_graph <- renderPlot({
    req(input$wstart_date) 
    plot <- weekly_graph(input$wstart_date)
    if (!is.null(plot)) {
      return(plot)
    } else {
      return(NULL)
    }
  })
  
  #CI graphs
  output$crop_density_plot <- renderPlot({
    crop_plot <- density_plot("crop", "Crop", NDVI_data, CI_csv, most_recent_data)
    print(crop_plot)
  })
  
  output$forest_density_plot <- renderPlot({
    forest_plot <- density_plot("forest", "Forest", NDVI_data, CI_csv, most_recent_data)
    print(forest_plot)
  })
  
  output$grassland_density_plot <- renderPlot({
    grassland_plot <- density_plot("grassland", "Grassland", NDVI_data, CI_csv, most_recent_data)
    print(grassland_plot)
  })
  
  output$uh_density_plot <- renderPlot({
    uh_plot <- density_plot("urban-high", "Urban-High", NDVI_data, CI_csv, most_recent_data)
    print(uh_plot)
  })
  
  output$um_density_plot <- renderPlot({
    um_plot <- density_plot("urban-medium", "Urban-Medium", NDVI_data, CI_csv, most_recent_data)
    print(um_plot)  
  })
  
  output$ul_density_plot <- renderPlot({
    ul_plot <- density_plot("urban-low", "Urban-Low", NDVI_data, CI_csv, most_recent_data)
    print(ul_plot)  
  })
  
  output$uo_density_plot <- renderPlot({
    uo_plot <- density_plot("urban-open", "Urban-Open", NDVI_data, CI_csv, most_recent_data)
    print(uo_plot)
  })

  #########forest#########
  output$for_gallery <- renderUI({
    graphic_return(for_files, "for")
    do.call(fluidRow, img_list)
  })
  
  graphic_formatting(for_files, "for", "forest", output)
  
  ###########crop###############
  
  output$crop_gallery <- renderUI({
    graphic_return(crop_files, "crop")
    do.call(fluidRow, img_list)
  })
  
  graphic_formatting(for_files, "crop", "crop", output)
###################grass#########
  output$grass_gallery <- renderUI({
    graphic_return(grass_files, "grass")
    do.call(fluidRow, img_list)
  })
  
  graphic_formatting(for_files, "grass", "grass", output)
#############urban##########
  output$uh_gallery <- renderUI({
    graphic_return(uh_files, "uh")
    do.call(fluidRow, img_list)
  })
  
  graphic_formatting(for_files, "uh", "urban-high", output)
  
  output$um_gallery <- renderUI({
    graphic_return(um_files, "um")
    do.call(fluidRow, img_list)
  })
  
  graphic_formatting(for_files, "um", "urban-medium", output)
  
  
  output$ul_gallery <- renderUI({
    graphic_return(ul_files, "ul")
    do.call(fluidRow, img_list)
  })
  
  graphic_formatting(for_files, "ul", "urban-low", output)
  
  output$uo_gallery <- renderUI({
    graphic_return(uo_files, "uo")
    do.call(fluidRow, img_list)
  })
  
  graphic_formatting(for_files, "uo", "urban-open", output)
  
  sliderValues <- reactive({
    
    data.frame(
      Name = c("Integer",
               "Decimal",
               "Range",
               "Custom Format",
               "Animation"),
      Value = as.character(c(input$integer,
                             input$decimal,
                             paste(input$range, collapse = " "),
                             input$format,
                             input$animation)),
      stringsAsFactors = FALSE)
    
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
 

}

# Run the application
shinyApp(ui = ui, server = server)
