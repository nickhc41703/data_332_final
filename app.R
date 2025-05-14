# app.R
library(shiny)
library(dplyr)
library(plotly)
library(tidyr)
library(leaflet)
library(sf)    
library(tidycensus)
library(tigris)    
library(stringr)
options(tigris_use_cache = TRUE) 


heart_data            <- readRDS(file.path("DataClean","heart.rds"))
heart_data_no_nans_22 <- readRDS(file.path("DataClean","heart_2022_no_nans.rds"))
heart_data_deaths <- read.csv(file.path("data","heart_disease_mortality.csv"))

census_api_key("aee2fe927e422686315c6280259ad4f55cc12333", install = FALSE)
Sys.setenv(CENSUS_API_KEY = "aee2fe927e422686315c6280259ad4f55cc12333")
counties_sf <- tigris::counties(cb = TRUE, class = "sf", year = 2022) %>%
  st_transform(crs = 4326)

deaths_by_county <- heart_data_deaths %>%
  filter(GeographicLevel == "County") %>%
  mutate(
    LocationID = as.integer(LocationID),
    GEOID = str_pad(LocationID, width = 5, pad = "0")
  ) %>%
  select(GEOID, DeathRate = Data_Value)

counties_death <- counties_sf %>%
  left_join(deaths_by_county, by = "GEOID")

income_by_county <- get_acs(
  geography = "county",
  variables = "B19013_001", 
  year      = 2021,
  cache_table = TRUE
)

counties_income <- counties_sf %>%
  left_join(income_by_county %>% select(GEOID, estimate), by = "GEOID")

pal_income <- colorNumeric(
  palette  = "YlOrRd",
  domain   = counties_income$estimate,
  na.color = "transparent"
)


datasets <- list(
  "Raw"     = heart_data,
  "No NaNs" = heart_data_no_nans_22
)

counties_sf <- tigris::counties(cb = TRUE, class = "sf", year = 2022)

ui <- fluidPage(
  titlePanel("Heart Disease Data Analysis"),
  
  tabsetPanel(id="which_tab", type="tabs",
              tabPanel("Raw"),
              tabPanel("No NaNs"),
              tabPanel("Compare"),
              tabPanel("Map") 
  ),
  
  conditionalPanel(
    "input.which_tab != 'Compare' && input.which_tab != 'Map'",
    sidebarLayout(
      sidebarPanel(
        uiOutput("cat_selector")
      ),
      mainPanel(
        plotlyOutput("singleChart", height="500px")
      )
    )
  ),
  
  conditionalPanel(
    "input.which_tab == 'Compare'",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "cat_compare", "Category (common to both):",
          choices = intersect(
            names(heart_data)[sapply(heart_data,  function(x) is.factor(x)||is.character(x))],
            names(heart_data_no_nans_22)[sapply(heart_data_no_nans_22, function(x) is.factor(x)||is.character(x))]
          )
        )
      ),
      mainPanel(
        plotlyOutput("compareChart", height="500px")
      )
    )
  ),
  
  conditionalPanel(
    "input.which_tab == 'Map'",
    fluidRow(
      column(3,
             selectInput("map_var", "Map variable:",
                         choices = c("Median Income" = "income",
                                     "Heart-Disease Death Rate" = "death"))
      ),
      column(9,
             leafletOutput("usMap", height = "600px")
      )
    )
  )
)

server <- function(input, output, session) {
  
  selected_df <- reactive({
    datasets[[input$which_tab]]
  })
  
  output$cat_selector <- renderUI({
    df <- selected_df()
    cats <- names(df)[sapply(df, function(x) is.factor(x)||is.character(x))]
    selectInput("cat_single", "Categorical variable:", choices = cats)
  })
  
  output$singleChart <- renderPlotly({
    df   <- selected_df()
    var  <- input$cat_single
    req(var)
    df2 <- df %>%
      group_by(.data[[var]]) %>%
      summarise(count = n(), .groups = "drop")
    
    plot_ly(df2, x = ~ .data[[var]], y = ~ count, type="bar",
            text = ~ count, textposition="auto") %>%
      layout(
        title = paste(input$which_tab, "–", var),
        xaxis = list(title = var, tickangle = -45),
        yaxis = list(title = "Count"),
        margin = list(b = 100)
      )
  })
  
  output$compareChart <- renderPlotly({
    var <- input$cat_compare
    req(var)
    
    raw_counts    <- heart_data %>%
      group_by(.data[[var]]) %>% summarise(raw = n(), .groups="drop")
    no_nans_counts <- heart_data_no_nans_22 %>%
      group_by(.data[[var]]) %>% summarise(no_nans = n(), .groups="drop")
    
    df_cmp <- full_join(raw_counts, no_nans_counts,
                        by = var) %>%
      replace_na(list(raw = 0, no_nans = 0))
    
    plot_ly(df_cmp, x = ~ .data[[var]], y = ~ raw, type="bar", name="Raw") %>%
      add_trace(y = ~ no_nans, name = "No NaNs") %>%
      layout(
        barmode = "group",
        title   = paste("Comparison –", var),
        xaxis   = list(title = var, tickangle = -45),
        yaxis   = list(title = "Count"),
        margin  = list(b = 100)
      )
  })
  
  map_sf <- reactive({
    if (input$map_var == "income") {
      df <- counties_income      %>% rename(MapValue = estimate)
      legend_title <- "Median Income"
    } else {
      df <- counties_death       %>% rename(MapValue = DeathRate)
      legend_title <- "Heart-Disease Death Rate"
    }
    list(sf = df, legend = legend_title)
  })
  
  pal <- reactive({
    colorNumeric("YlOrRd",
                 domain   = map_sf()$sf$MapValue,
                 na.color = "transparent")
  })
  
  output$usMap <- renderLeaflet({
    leaflet(map_sf()$sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor       = ~pal()(MapValue),
        fillOpacity     = 0.7,
        weight          = 0.3,
        color           = "white",
        label           = ~sprintf(
          "<strong>%s</strong><br/>%s: %s",
          NAMELSAD,
          map_sf()$legend,
          scales::comma(MapValue)
        ) %>% lapply(htmltools::HTML),
        highlightOptions = highlightOptions(
          weight       = 2, color = "#666",
          fillOpacity  = 0.9, bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal      = pal(),
        values   = ~MapValue,
        title    = map_sf()$legend,
        position = "bottomright"
      )
  })
  
}

shinyApp(ui, server)
