library(shiny)

# 1. Create a small mock dataset
municipal_data_merged <- data.frame(
  REGIONE = c("Lombardia", "Lombardia", "Lombardia", "Lombardia", "Lazio", "Lazio", "Lazio", "Lazio"),
  PROVINCIA = c("Milano", "Milano", "Bergamo", "Bergamo", "Roma", "Roma", "Latina", "Latina"),
  COMUNE = c("Milano", "Sesto San Giovanni", "Bergamo", "Treviglio", "Roma", "Tivoli", "Latina", "Formia"),
  stringsAsFactors = FALSE
)

# 2. UI
ui <- fluidPage(
  titlePanel("Italian Municipalities Selector"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("region_select", "Region:", choices = c("", unique(municipal_data_merged$REGIONE))),
      selectizeInput("province_select", "Province:", choices = c("", unique(municipal_data_merged$PROVINCIA))),
      selectizeInput("comune_select", "Municipality:", choices = c("", unique(municipal_data_merged$COMUNE)))
    ),
    mainPanel(
      h4("Current Selections:"),
      verbatimTextOutput("selection_status")
    )
  )
)

# 3. Server
server <- function(input, output, session) {
  
  # Region selected -> Filter Province & Comune
  observeEvent(input$region_select, {
    if (input$region_select == "") {
      updateSelectizeInput(session, "province_select", choices = c("", unique(municipal_data_merged$PROVINCIA)))
      updateSelectizeInput(session, "comune_select", choices = c("", unique(municipal_data_merged$COMUNE)))
    } else {
      filtered <- municipal_data_merged[municipal_data_merged$REGIONE == input$region_select, ]
      updateSelectizeInput(session, "province_select", choices = c("", unique(filtered$PROVINCIA)), selected = input$province_select)
      updateSelectizeInput(session, "comune_select", choices = c("", unique(filtered$COMUNE)), selected = input$comune_select)
    }
  }, ignoreInit = TRUE)
  
  # Province selected -> Auto-select Region, filter Comune
  observeEvent(input$province_select, {
    if (input$province_select != "") {
      filtered <- municipal_data_merged[municipal_data_merged$PROVINCIA == input$province_select, ]
      updateSelectizeInput(session, "region_select", selected = unique(filtered$REGIONE))
      updateSelectizeInput(session, "comune_select", choices = c("", unique(filtered$COMUNE)), selected = input$comune_select)
    }
  }, ignoreInit = TRUE)
  
  # Comune selected -> Auto-select Region & Province
  observeEvent(input$comune_select, {
    if (input$comune_select != "") {
      selected_row <- municipal_data_merged[municipal_data_merged$COMUNE == input$comune_select, ]
      updateSelectizeInput(session, "region_select", selected = unique(selected_row$REGIONE))
      
      prov_choices <- unique(municipal_data_merged$PROVINCIA[municipal_data_merged$REGIONE == unique(selected_row$REGIONE)])
      updateSelectizeInput(session, "province_select", 
                           choices = c("", prov_choices),
                           selected = unique(selected_row$PROVINCIA))
    }
  }, ignoreInit = TRUE)
  
  # Output for verification
  output$selection_status <- renderPrint({
    list(
      Region = input$region_select,
      Province = input$province_select,
      Municipality = input$comune_select
    )
  })
}

shinyApp(ui, server)