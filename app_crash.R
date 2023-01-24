library(tidyverse)
library(lubridate)
library(RSocrata)
library(shiny)
library(rgdal)
library(DT)
library(dygraphs)
library(xts)
library(leaflet)
library(plotly)

#years_ago <- today() - years(2)
#crash_url <- glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_ago}'")
#crash_raw <- as_tibble(read.socrata(crash_url))
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

## SHINY ABAIXO

#data <- read.csv("data/data.csv")
#map <- readOGR("data/fe_2007_39_county/fe_2007_39_county.shp")


teste <- levels(crash_2$injuries)

ui <- fluidPage(
  titlePanel("Shiny app com um ggplot"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "injuries",
        label = "injuries",
        choices = teste
      )
    ),
    mainPanel(
      plotOutput("grafico")
    )
  )
)

server <- function(input, output, session) {
  output$grafico <- renderPlot({
    if (input$injuries == "none") {
    crash_none %>%
      filter(latitude > 0) %>%
      ggplot(aes(longitude, latitude)) +
      geom_point(size = 0.5, alpha = 0.4) +
      labs(color = NULL) +
#      scale_color_manual(values = c("deeppink4", "gray80")) +
      coord_fixed() }
    else {
      crash_inj %>%
        filter(latitude > 0) %>%
        ggplot(aes(longitude, latitude)) +
        geom_point(size = 0.5, alpha = 0.4) +
        labs(color = NULL) +
#        scale_color_manual(values = c("deeppink4", "gray80")) +
        coord_fixed() 
    }
  })
}

shinyApp(ui, server)
