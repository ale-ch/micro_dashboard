library(shiny)
library(tmap)
library(plotly)
library(dplyr)

# source('/Volumes/T7 Shield/FRES/DB_Comunale/micro_dashboard/LOAD_DATA.r')
source('/Volumes/T7 Shield/FRES/DB_Comunale/micro_dashboard/tests/LOAD_DATA_TEST.r')

tmap_mode("view")

get_dfs <- function() {
  all_names <- ls(envir = .GlobalEnv)
  sampled_names <- grep("_sampled_map$", all_names, value = TRUE)
  objs <- mget(sampled_names, envir = .GlobalEnv)
  objs[sapply(objs, inherits, what = "sf")]
}

ui <- fluidPage(
  titlePanel("Municipal data dashboard"),
  
  tabsetPanel(
    
    tabPanel("Map",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("var_select"),
                 uiOutput("year_select_ui"),
                 uiOutput("comune_select_wrapper_1")
               ),
               mainPanel(
                 tmapOutput("map")
               )
             )
    ),
    
    tabPanel("Time series",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("ts_var_select"),
                 uiOutput("date_range_ui"),
                 uiOutput("comune_select_wrapper")
               ),
               mainPanel(
                 plotlyOutput("ts_plot")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  # MAP
  output$var_select <- renderUI({
    req(municipal_data_merged)
    selectInput("variable", "Select variable",
                choices = names(municipal_data_merged))
  })
  
  output$year_select_ui <- renderUI({
    
    req(municipal_data_merged)
    req("year" %in% names(municipal_data_merged))
    
    yrs <- sort(unique(municipal_data_merged$year))
    
    selectInput(
      inputId = "year_select",
      label = "Select year",
      choices = yrs,
      selected = NULL,
      multiple = FALSE,
      selectize = TRUE,
      width = NULL,
      size = NULL
    )
  })
  
  
  # 1. Initialize the empty widget
  output$comune_select_wrapper_1 <- renderUI({
    selectizeInput(
      "selected_comune_map", 
      "Search and Select comune:", 
      choices = sort(unique(municipal_data_merged$COMUNE)), # Leave empty initially
      multiple = TRUE,
      options = list(
        placeholder = 'Type to search...',
        loadThrottle = 100 # Wait 300ms after typing stops before searching
      )
    )
  })
  
  # 2. Update the choices from the server side
  observeEvent(municipal_data_merged, {
    req(municipal_data_merged)
    choices <- sort(unique(municipal_data_merged$COMUNE))
    
    updateSelectizeInput(
      session, 
      "selected_comune", 
      choices = choices, 
      server = TRUE # This enables fast searching/suggestions as you type
    )
  })
  
  
  
  output$map <- renderTmap({
    req(municipal_data_merged, input$variable, input$year_select)
    
    if (is.null(input$selected_comune_map)) {
      tm_shape(municipal_data_merged %>% filter(year == input$year_select)) +
        tm_polygons(input$variable)
    } else {
      tm_shape(municipal_data_merged %>% filter(year == input$year_select, COMUNE == input$selected_comune_map)) +
        tm_polygons(input$variable)
    }
    
    
  })
  
  # TIME SERIES
  output$ts_var_select <- renderUI({
    req(municipal_data_merged)
    numeric_vars <- names(municipal_data_merged)[sapply(municipal_data_merged, is.numeric)]
    selectInput("ts_variable", "Select variable",
                choices = numeric_vars)
  })
  
  output$date_range_ui <- renderUI({
    req(municipal_data_merged)
    req("year" %in% names(municipal_data_merged))
    
    yrs <- sort(unique(municipal_data_merged$year))
    
    sliderInput(
      "date_range",
      "Select year range",
      min = min(yrs),
      max = max(yrs),
      value = c(min(yrs), max(yrs)),
      step = 1,
      sep = ""
    )
  })

  
  
  
  # 1. Initialize the empty widget
  output$comune_select_wrapper <- renderUI({
    selectizeInput(
      "selected_comune", 
      "Search and Select comune:", 
      choices = sort(unique(municipal_data_merged$COMUNE)), # Leave empty initially
      multiple = TRUE,
      options = list(
        placeholder = 'Type to search...',
        loadThrottle = 100 # Wait 300ms after typing stops before searching
      )
    )
  })
  
  # 2. Update the choices from the server side
  observeEvent(municipal_data_merged, {
    req(municipal_data_merged)
    choices <- sort(unique(municipal_data_merged$COMUNE))
    
    updateSelectizeInput(
      session, 
      "selected_comune", 
      choices = choices, 
      server = TRUE # This enables fast searching/suggestions as you type
    )
  })
  
  output$ts_plot <- renderPlotly({
    # Use req() to ensure selected_comune exists before plotting
    req(municipal_data_merged, input$ts_variable, input$date_range, input$selected_comune)
    
    df <- municipal_data_merged %>%
      filter(
        year >= input$date_range[1],
        year <= input$date_range[2],
        COMUNE %in% input$selected_comune
      ) %>%
      arrange(year)
    
    plot_ly(
      df,
      x = ~year,
      y = as.formula(paste0("~`", input$ts_variable, "`")),
      color = ~COMUNE,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(
        xaxis = list(title = "Year", tickformat = "d"),
        yaxis = list(title = input$ts_variable)
      )
  })
}

shinyApp(ui, server)