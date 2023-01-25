library(shiny)
library(rgdal)
library(lubridate)
library(DT)
library(dygraphs)
library(xts)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(tidyverse)
library(RSocrata)
library(plotly)

years_ago <- today() - years(2)
crash_url <- glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_ago}'")
crash_raw <- as_tibble(read.socrata(crash_url))
crash <- crash_raw %>%
  arrange(desc(crash_date)) %>%
  transmute(
    injuries = if_else(injuries_total > 0, "injuries", "none"),
    crash_date,
    crash_hour,
    report_type = if_else(report_type == "", "UNKNOWN", report_type),
    num_units,
    posted_speed_limit,
    weather_condition,
    lighting_condition,
    roadway_surface_cond,
    first_crash_type,
    trafficway_type,
    prim_contributory_cause,
    latitude, longitude
  ) %>%
  mutate(injuries = as.factor(injuries)) %>%
  na.omit()

crash_2 <- crash %>%
  select(latitude, longitude, injuries)

#ggplot(data = crash, aes(longitude, latitude, color = injuries)) +
#  geom_point(size = 0.5, alpha = 0.4)


crash_none <- crash_2 %>%
  filter(latitude > 0,
         injuries == "none")

crash_inj <- crash_2 %>%
  filter(latitude > 0,
         injuries == "injuries")

teste <- levels(crash_2$injuries)


n1 <- nrow(crash_inj)
n2 <- nrow(crash_none)
crash_inj_2 <- sample_n(crash_inj, n1*0.40)
crash_none_2 <- sample_n(crash_none, n2*0.05)

## MAPA LEAFLET

#n <- nrow(crash_2)

#crash_2_10 <- sample_n(crash_2, n*0.10)

#crash_2_10 %>%
#  leaflet() %>%
#  addProviderTiles(providers$CartoDB.Positron) %>% 
#  addHeatmap(lng=~longitude,lat=~latitude,intensity=~injuries,max=100,radius=20,blur=10)


#crash_inj %>%
#  leaflet() %>%
#  addProviderTiles(providers$CartoDB.Positron) %>% 
#  addHeatmap(lng=~longitude,lat=~latitude,intensity=~injuries,max=100,radius=20,blur=10)

#crash_none %>%
#  leaflet() %>%
#  addProviderTiles(providers$CartoDB.Positron) %>% 
#  addHeatmap(lng=~longitude,lat=~latitude,intensity=~injuries,max=100,radius=20,blur=10)

# APPZADA

ui <- fluidPage(
  titlePanel("Lista 2 - Gabriel (291440), Thomas (261057)"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "injuries",
        label = "injuries",
        choices = teste
      )
    ),
    mainPanel(
      leafletOutput(outputId = "map")
    )
  )
)

# server()
server <- function(input, output) {
  output$map <- renderLeaflet({
    if (input$injuries == "none") {
      crash_none_2 %>%
        leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addHeatmap(lng=~longitude,lat=~latitude,intensity=~injuries,max=100,radius=20,blur=10)
    }
    else{
      crash_inj_2 %>%
        leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addHeatmap(lng=~longitude,lat=~latitude,intensity=~injuries,max=100,radius=20,blur=10)
    }
  })
}

# shinyApp()
shinyApp(ui = ui, server = server)