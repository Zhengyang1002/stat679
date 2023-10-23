library(tidyverse)
library(ggplot2)
library(shiny)
library(dplyr)
library(stringr)
library(plotly)
library(ggthemes)
library(ggHoriPlot) 
library(sf)
library(leaflet)
library(ggmap)
library(glue)
library(bslib)
library(tmap)
library(terra)
library(ceramic)
power_plants <- st_read('https://raw.githubusercontent.com/krisrs1128/stat992_f23/main/exercises/ps2/power_plants.geojson')
power_plants$longitude <- as.numeric(sapply(str_extract_all(power_plants$geometry, "-?\\d+\\.\\d+"), function(x) x[1]))
power_plants$latitude <- as.numeric(sapply(str_extract_all(power_plants$geometry, "-?\\d+\\.\\d+"), function(x) x[2]))
scatterplot <- function(df, selected_) {
  df %>%
    mutate(selected = selected_) %>%
    ggplot() +
    geom_point(
      aes(
        longitude, latitude, 
        col = primary_fuel, 
        alpha = as.numeric(selected),
        size = as.numeric(log_capacity) + 3 * as.numeric(selected)
      )
    ) +
    scale_alpha(range = c(0.1, 1), guide = guide_legend(title = "Selected")) +
    scale_color_brewer(palette = "Set2") +
    scale_size(range = c(0.1, 3.0), guide = guide_legend(title = "Log Capacity")) +
    scale_x_continuous(
      breaks = seq(-100, -80, by = 5),
      labels = seq(-100, -80, by = 5),  
      limits = c(-100, -80),  
      expand = c(0, 0) 
    )+
    theme(
      legend.position = "bottom", 
      legend.box = "horizontal"   
    )+
    theme_void()
}

overlay_histogram <- function(df, selected_) {
  sub_df <- filter(df, selected_)
  ggplot(df, aes(log_capacity, fill = primary_fuel)) +
    geom_histogram(alpha = 0.3) +
    geom_histogram(data = sub_df) +
    scale_y_continuous(expand = c(0, 0, 0.1, 0)) +
    scale_fill_brewer(palette = "Set2", guide = "none")
}

filter_df <- function(df, selected_) {
  filter(df, selected_) %>%
    select(name, primary_fuel,capacity_mw)
}

ui <- fluidPage(
  h3("Midwestern Power Plants"),
  fluidRow(
    column(6,
           plotOutput("histogram", brush = brushOpts("plot_brush", direction = "x"), height = 300),
           dataTableOutput("table"),
    ),
    column(6, plotOutput("map", brush = "plot_brush", height = 600))
  ),
  theme = bs_theme(bootswatch = "minty")
)

server <- function(input, output) {
  selected <- reactiveVal(rep(TRUE, nrow(power_plants)))
  observeEvent(input$plot_brush, {
    selected(brushedPoints(power_plants, input$plot_brush, allRows = TRUE)$selected_)
  })
  
  output$histogram <- renderPlot(overlay_histogram(power_plants, selected()))
  output$map <- renderPlot(scatterplot(power_plants, selected()))
  output$table <- renderDataTable(filter_df(power_plants, selected()))
  
}

shinyApp(ui, server)