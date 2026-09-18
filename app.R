library(shiny)
library(tidyverse)
library(DBI)
library(odbc)
library(lubridate)
library(here)

source(here('code', 'helper_functions.R'))

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
  tolower() %>%
  c(., 'unknown')

tbl_station <- tbl(con, dbo('station')) %>%
  collect() %>%
  mutate(plot_order = factor(plot_order, levels = sort(plot_order)))


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("FishTracks Real Time Prototype"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectizeInput(
        inputId = 'species',
        label = 'Select spp.',
        choices = all_spp,
        selected = c(
          'silver carp',
          'silver carp/bighead carp',
          'bighead carp',
          'grass carp',
          'unknown'
        ),
        multiple = TRUE
      ),
      selectizeInput(
        inputId = 'river_name',
        label = 'Select river',
        choices = unique(tbl_station$river_name),
        selected = setdiff(unique(tbl_station$river_name), 'Sandusky River'),
        multiple = TRUE
      ),
      selectInput(
        inputId = 'lookback',
        label = 'Select lookback period',
        choices = c('1 day' = 1, '1 week' = 7, '1 month' = 30, 'Max' = 'Max'),
        selected = '1'
      ),
      selectInput(
        inputId = 'timeagg',
        label = 'Aggregate detections',
        choice = c('5 minutes', '1 hour', '1 day'),
        selected = '1 hour'
      )
    ),

    # Show a plot
    mainPanel(width = 10, plotOutput("plot", height = 800))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  time_threshold <- reactive({
    req(input$lookback)

    if (input$lookback == 'Max') {
      tbl(con, dbo('event')) %>%
        summarize(oldest_record = min(TimeStamp, na.rm = TRUE)) %>%
        pull(oldest_record)
    } else {
      as_datetime(now() - days(input$lookback))
    }
  })

  data_timefilter <- reactive({
    req(time_threshold())

    time_threshold_nonr <- time_threshold()

    tbl(con, dbo('event_animal')) %>%
      filter(TimeStamp >= time_threshold_nonr)
  })

  data_riverfilter <- reactive({
    req(data_timefilter())

    selected_rivers_nonr <- input$river_name

    data_timefilter() %>%
      left_join(tbl(con, dbo('station'))) %>%
      filter(river_name %in% selected_rivers_nonr)
  })

  data_sppfilter <- reactive({
    req(data_riverfilter())
    req(input$species)
    selected_species_nonr <- input$species

    data_riverfilter() %>%
      left_join(tbl(con, dbo('tag'))) %>%
      mutate(common_name_e = coalesce(common_name_e, 'unknown')) %>%
      filter(common_name_e %in% selected_species_nonr)
  })

  data_timeagg <- reactive({
    req(data_sppfilter())
    req(input$timeagg)

    data_sppfilter() %>%
      aggregate_detections_lazy(unit = input$timeagg) %>%
      collect()
  })

  data_timeseries_plot <- reactive({
    station_labels_ordered <- data_riverfilter() %>%
      select(station_label, plot_order) %>%
      distinct() %>%
      collect() %>%
      arrange(plot_order) %>%
      pull(station_label)

    data_timeagg() %>%
      left_join(tbl_station) %>%
      group_by(TimeStamp_binned, station_label, common_name_e) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      left_join(tbl_station) %>%
      mutate(
        station_label = factor(
          station_label,
          levels = station_labels_ordered
        )
      )
  })

  output$plot <- renderPlot({
    req(data_timeseries_plot())

    data_timeseries_plot() %>%
      ggplot(aes(
        x = TimeStamp_binned,
        y = n,
        color = common_name_e,
        alpha = .7
      )) +
      geom_point(pch = 16) +
      geom_line() +
      facet_wrap(~station_label, scales = 'fixed', drop = FALSE) +
      labs(
        x = 'Time',
        y = paste0('Number of detections (per ', input$timeagg, ')'),
        color = 'Species'
      ) +
      theme_minimal(base_size = 15) +
      scale_y_continuous(
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.15)),
        labels = scales::label_number(accuracy = 1)
      ) +
      scale_x_datetime(
        limits = c(time_threshold(), now()),
        expand = c(0.05, 0.05),
        date_breaks = case_when(
          input$lookback == 1 ~ '12 hours',
          input$lookback == 7 ~ '3 days',
          input$lookback == 30 ~ '10 days',
          input$lookback == 'Max' ~ '1 month',
          TRUE ~ '1 month'
        ),

        date_labels = case_when(
          input$lookback == 1 ~ "%m/%d %H:%M",
          input$lookback == 7 ~ "%b %d",
          input$lookback == 30 ~ "%b %Y",
          input$lookback == 'Max' ~ "%b %Y",
          TRUE ~ "%b %Y"
        )
      ) +
      scale_alpha_identity()
  })
}
# Run the application
shinyApp(ui = ui, server = server)
