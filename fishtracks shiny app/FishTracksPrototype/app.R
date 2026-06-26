library(shiny)
library(tidyverse)
library(DBI)
library(odbc)
library(lubridate)
# schema setting helper
dbo <- function(name) {
  Id(schema = "dbo", table = name)
}

con <- dbConnect(odbc::odbc(), "Fish_Tracks_Real_Time")

all_spp <- tbl(con, dbo('tag')) %>%
  select(common_name_e) %>%
  collect() %>%
  unique() %>%
  pull() %>%
  tolower()

tbl_station <- tbl(con, dbo('station')) %>%
  collect()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("FishTracks Real Time Prototype"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = 'species', label = 'Select spp.', choices = all_spp, selected = 'silver carp', multiple = TRUE),
            selectizeInput(inputId = 'river_name', label = 'Select river', choices = unique(tbl_station$river_name), selected = unique(tbl_station$river_name), multiple = TRUE),
            selectInput(inputId = 'days', label = 'Select lookback (days)', choices = c('1', '3', '7', '14'), selected = '1')
        ),

        # Show a plot
        mainPanel(
           plotOutput("plot", height = 900)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
  time_threshold <- reactive({
    req(input$days)
    today() - days(input$days)
    })
  
  data_timefilter <- reactive({
    
    req(time_threshold())
    
    time_threshold_nonr <- time_threshold()
    
    tbl(con, dbo('event')) %>%
      filter(TimeStamp >= time_threshold_nonr ) %>%
      collect() %>%
      pivot_longer(
        cols = matches("^TagID(_\\d+)?$|^TagIDTimeStamp_\\d+$"),
        names_to = c(".value", "idx"),
        names_pattern = "(TagID|TagIDTimeStamp)_?(\\d+)"
      ) %>%
      filter(!is.na(TagID) & TagID != "")
  })
  
  
  data_riverfilter <- reactive({
    
    req(data_timefilter())
    
    selected_rivers_nonr <- input$river_name
    
    tbl(con, dbo('station')) %>%
      filter(river_name %in% selected_rivers_nonr) %>%
      collect() %>%
      right_join(data_timefilter())
    
  })
  
  data_sppfilter <- reactive({
    
    req(data_riverfilter())
    req(input$species)
    selected_species_nonr <- input$species
      
    tbl(con, dbo('tag')) %>%
      filter(common_name_e %in% selected_species_nonr) %>%
      mutate(TagID = paste(tag_code_space, tag_id_code, sep = '-')) %>%
      collect() %>%
      right_join(data_riverfilter()) %>%
      mutate(common_name_e = coalesce(common_name_e, 'unknown'))
  })
  
    

  
  
    output$plot <- renderPlot({
        req(data_sppfilter())
      
      data_sppfilter() %>%
        left_join(tbl_station) %>%
        group_by(TimeStamp, station_name, common_name_e) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        ggplot(aes(x = TimeStamp, y = n, color = common_name_e)) +
        geom_point() +
        geom_line(alpha = .7) +
        facet_wrap(~station_name, scales = 'fixed') +
        labs(x = 'Time', y = 'Number of detections (per 5-min period)', color = 'Species' ) +
        theme_minimal(base_size = 15) +
        scale_y_continuous(limits = c(0,NA), expand = c(0,0)) 
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
