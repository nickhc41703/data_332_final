library(shiny)
library(dplyr)
library(plotly)


heart_data <- readRDS(file.path("DataClean", "heart.rds"))
heart_data_cleaned_22 <- readRDS(file.path("DataClean", "heart_2020_cleaned.rds"))
heart_data_no_nans_22 <- readRDS(file.path("DataClean", "heart_2022_no_nans.rds"))
heart_data_with_nans_22 <- readRDS(file.path("DataClean", "heart_2022_with_nans.rds"))


ui <- fluidPage(
  titlePanel("Heart App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("which_ds", "Dataset:", 
                  choices = c("Raw"="heart_data", "Cleaned2020"="heart_data_cleaned_22")),
      uiOutput("catVar_ui")
    ),
    mainPanel(
      tableOutput("data_preview"),
      plotlyOutput("barChart")
    )
  )
)

server <- function(input, output, session) {
  output$debug_fs <- renderPrint({
    cat("WD: ", getwd(), "\n")
    cat("Files here:\n")
    print(list.files())
    cat("DataClean dir:\n")
    if (dir.exists("DataClean")) print(list.files("DataClean")) else cat("<<no DataClean folder>>")
  })
  
  output$catVar_ui <- renderUI({
    df <- switch(input$which_ds,
                 heart_data            = heart_data,
                 heart_data_cleaned_22 = heart_data_cleaned_22)
    cats <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    if (length(cats)==0) {
      return(helpText("⚠️ No factor/char columns found"))
    }
    selectInput("catVar", "Categorical var:", choices = cats)
  })
  
  selected_df <- reactive({
    switch(input$which_ds,
           heart_data            = heart_data,
           heart_data_cleaned_22 = heart_data_cleaned_22)
  })
  
  output$data_preview <- renderTable({
    head(selected_df(), 5)
  })
  
  summary_df <- reactive({
    req(input$catVar)
    selected_df() %>%
      group_by(.data[[input$catVar]]) %>%
      summarise(count = n(), .groups="drop")
  })
  
  output$barChart <- renderPlotly({
    df2 <- summary_df()
    req(nrow(df2) > 0)
    
    plot_ly(
      data = df2,
      x    = ~ .data[[input$catVar]],
      y    = ~ count,
      type = "bar",
      text = ~ count,
      textposition = "auto"
    ) %>%
      layout(
        xaxis = list(title = input$catVar),
        yaxis = list(title = "Count"),
        margin = list(b = 100)
      )
  })
  
}

shinyApp(ui, server)
