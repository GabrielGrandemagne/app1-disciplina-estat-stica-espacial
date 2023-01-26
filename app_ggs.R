## app.R ##
library(shiny)
library(shinydashboard)
library(rgdal)
library(lubridate)
library(DT)
library(dygraphs)
library(xts)
library(leaflet)
library(leaflet.extras)
library(dplyr)
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
  filter(latitude > 0) %>%
  select(latitude, longitude, injuries, crash_hour, weather_condition) %>%
  mutate(
    weather_condition = as.factor(weather_condition)
  )

teste <- levels(crash_2$injuries)

## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Crashes Chicago"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Heat_Map", tabName = "heatmap", icon = icon("dashboard")),
      menuItem("Scatter", tabName = "scatter", icon = icon("th"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
    tabItem(tabName = "heatmap",
    fluidRow(
      box(
        title = "Heat Map", status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        leafletOutput("plot1", height = 600)
        ),
      
      box(
        title = "Filtros", status = "primary", solidHeader = TRUE,
        selectInput(
          "caixa",
          label = "Feridos ou Não Feridos",
          choices = teste
        )
      ),
      box(
        title = "Tamanho da amostra", status = "primary", solidHeader = TRUE,
        sliderInput("slider", "Number of observations:", 1, 214965, 8000)
      )
    )
   ),
  tabItem(tabName = "scatter",
          fluidRow(
            box(
              title = "Scatter Plot", status = "warning", solidHeader = TRUE,
              collapsible = TRUE,
            plotOutput("grafico", height = 700, width = 600),
            ),
            box(
              title = "Feridos ou Não Feridos", status = "warning", solidHeader = TRUE,
              selectInput(
                "caixa2",
                label = "Feridos",
                choices = teste
              )
            ),
            box(
              title = "Tamanho da amostra", status = "warning", solidHeader = TRUE,
              sliderInput("slider2", "Number of observations:", 1, 214965, 8000)
            )
          )
  )
 )
)
)

server <- function(input, output) {
  output$plot1 <- renderLeaflet({
    data <- crash_2 %>%
      filter(injuries == input$caixa)
    
    #n <- nrow(data)
    data2 <- sample_n(data, input$slider)
    
    data2 %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addHeatmap(lng=~longitude,lat=~latitude,intensity=~injuries,max=100,radius=20,blur=10)
  })
  
  output$grafico <- renderPlot({
    
    data <- crash_2 %>%
      filter(injuries == input$caixa2)
    
    #n <- nrow(data)
    data2 <- sample_n(data, input$slider2)
    
    data2 %>%
        ggplot(aes(longitude, latitude)) +
        geom_point(size = 0.5, alpha = 0.4) +
        labs(color = NULL) +
        #      scale_color_manual(values = c("deeppink4", "gray80")) +
        coord_fixed() 
  })
}

shinyApp(ui, server)