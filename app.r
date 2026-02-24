# app.R

library(shiny)
library(tmap)

source('/Volumes/T7 Shield/FRES/DB_Comunale/LOAD_DATA.r')

tmap_mode("view")  # interactive

# assume your spatial data frames are already loaded in the environment
# e.g. df1, df2, df3 (all sf objects)

#get_dfs <- function() {
#  objs <- mget(ls(envir = .GlobalEnv), envir = .GlobalEnv)
#  objs[sapply(objs, inherits, what = "sf")]
#}

get_dfs <- function() {
  all_names <- ls(envir = .GlobalEnv)
  sampled_names <- grep("_sampled_map$", all_names, value = TRUE)
  
  objs <- mget(sampled_names, envir = .GlobalEnv)
  objs[sapply(objs, inherits, what = "sf")]
}

ui <- fluidPage(
  titlePanel("Map viewer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select dataset",
                  choices = names(get_dfs())),
      
      uiOutput("var_select")
    ),
    
    mainPanel(
      tmapOutput("map")
    )
  )
)

server <- function(input, output, session) {
  
  datasets <- reactive({
    get_dfs()
  })
  
  df_selected <- reactive({
    req(input$dataset)
    datasets()[[input$dataset]]
  })
  
  output$var_select <- renderUI({
    req(df_selected())
    vars <- names(df_selected())
    selectInput("variable", "Select variable", choices = vars)
  })
  
  output$map <- renderTmap({
    req(df_selected(), input$variable)
    
    tm_shape(df_selected()) +
      tm_polygons(input$variable)
  })
}

shinyApp(ui, server)