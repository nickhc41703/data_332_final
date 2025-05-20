# app.R
library(shiny)
library(shinythemes)
library(dplyr)
library(plotly)
library(tidyr)
library(leaflet)
library(sf)    
library(tidycensus)
library(tigris)    
library(stringr)
library(RColorBrewer)
library(gridExtra)
library(glue)
library(tidymodels)
options(tigris_use_cache = TRUE) 


heart_data            <- readRDS(file.path("DataClean","heart.rds"))
heart_data_no_nans_22 <- readRDS(file.path("DataClean","heart_2022_no_nans.rds"))
heart_data_cleaned_20 <- readRDS(file.path("DataClean", "heart_2020_cleaned.rds"))
heart_data_deaths <- readRDS(file.path("DataClean","heart_disease_mortality.rds"))
model <- readRDS(file.path("DataClean", "heart_model_simple.rds"))

heart_data <- heart_data %>%
  rename(HadAngina = ChestPainType) %>%
  mutate(HadAngina = ifelse(HadAngina %in% c("TA", "ASY", "ATA"), "Yes", "No"))

heart_data <- heart_data %>%
  mutate(Sex = recode(Sex, "M" = "Male", "F" = "Female"))

age_min <- floor(min(heart_data$Age, na.rm = TRUE) / 5) * 5
age_max <- ceiling(max(heart_data$Age, na.rm = TRUE) / 5) * 5
breaks <- seq(age_min, age_max, by = 5)

heart_data$AgeCategory <- cut(heart_data$Age, breaks = breaks, labels = paste(head(breaks, -1), breaks[-1] - 1, sep = "-"), right = FALSE)

heart_data <- heart_data[, !names(heart_data) %in% "Age"]

general_table_1 <- heart_data %>%
  group_by(AgeCategory, Sex, HeartDisease) %>%
  summarise(Count = n(), .groups = "drop")

general_table_1 <- general_table_1 %>%
  mutate(HeartDisease = ifelse(HeartDisease == 1, "Yes", "No"))


general_table_2 <- heart_data_cleaned_20 %>%
  group_by(AgeCategory, Sex, HeartDisease) %>%
  summarise(Count = n(), .groups = "drop")

general_table_2 <- general_table_2 %>%
  mutate(AgeCategory = str_replace_all(AgeCategory, c(" or older" = "+")))

general_table_3 <- heart_data_no_nans_22 %>%
  group_by(AgeCategory, Sex, HadHeartAttack) %>%
  summarise(Count = n(), .groups = "drop")

age_order <- c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
               "55-59", "60-64", "65-69", "70-74", "75-79", "80+", "80 or older")

colnames(general_table_3)[colnames(general_table_3) == "HadHeartAttack"] <- "HeartDisease"

full_table <- bind_rows(general_table_1, general_table_2, general_table_3) %>%
  filter(!is.na(AgeCategory))
full_table$AgeCategory <- factor(full_table$AgeCategory, levels = age_order)

race_table   <- heart_data_cleaned_20 %>% group_by(Race,HeartDisease) %>% summarise(Count=n(),.groups="drop")
race_table_2 <- heart_data_no_nans_22 %>% group_by(RaceEthnicityCategory,HadHeartAttack) %>% summarise(Count=n(),.groups="drop") %>% rename(HeartDisease=HadHeartAttack)
state_table  <- heart_data_no_nans_22 %>% group_by(State,RaceEthnicityCategory) %>% summarise(Count=n(),.groups="drop")
Activity_table   <- heart_data_cleaned_20 %>% group_by(BMI,PhysicalActivity,HeartDisease) %>% summarise(Count=n(),.groups="drop")
substance_table  <- heart_data_cleaned_20 %>% group_by(Smoking,AlcoholDrinking,HeartDisease) %>% summarise(Count=n(),.groups="drop")


Check_up_table <- heart_data_no_nans_22 %>%
  group_by(HadHeartAttack, State, LastCheckupTime) %>%
  summarise(Count = n(), .groups = "drop")

colnames(Check_up_table)[colnames(Check_up_table) == "HadHeartAttack"] <- "HeartDisease"



census_api_key("aee2fe927e422686315c6280259ad4f55cc12333", install = TRUE, overwrite = TRUE)
Sys.setenv(CENSUS_API_KEY = "aee2fe927e422686315c6280259ad4f55cc12333")

counties_sf <- tigris::counties(cb = TRUE, class = "sf", year = 2022) %>%
  st_transform(crs = 4326)


deaths_by_county <- heart_data_deaths %>%
  filter(
    GeographicLevel  == "County",
    Stratification1  == "Overall",
    Stratification2  == "Overall"
  ) %>%
  mutate(
    GEOID     = str_pad(LocationID, 5, pad = "0"),
    DeathRate = Data_Value
  ) %>%
  select(GEOID, DeathRate)

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

datasets <- list(
  "Raw"     = heart_data,
  "No NaNs" = heart_data_no_nans_22
)

counties_combined <- counties_sf %>%
  left_join(deaths_by_county, by = "GEOID") %>%   
  left_join(income_by_county %>% select(GEOID, estimate),
            by = "GEOID") %>%                 
  rename(
    death_rate = DeathRate,
    income     = estimate
  )

counties_combined <- counties_combined %>%
  mutate(death_income_ratio = death_rate / (income / 1000)) %>%
  mutate(income_quintile = ntile(income, 5))

quintile_summary <- counties_combined %>%
  st_set_geometry(NULL) %>%     
  group_by(income_quintile) %>%
  summarise(mean_death = mean(death_rate, na.rm = TRUE))

counties_combined <- counties_combined %>%
  left_join(quintile_summary, by = "income_quintile")

myPurples <- c(
  "#f7fcfd", "#e0ecf4", "#bfd3e6",
  "#9ebcda", "#8c96c6", "#8c6bb1",
  "#88419d", "#810f7c", "#4d004b"
)


ui <- fluidPage(
  theme = shinytheme("superhero"), 
  titlePanel("Heart Disease Data Analysis"),
  
  tabsetPanel(id = "which_tab", type = "tabs",
              tabPanel("Introduction",
                      fluidRow(
                      column(12, h1("Heart Disease Analysis and Reflection"),
                             h3("The Why"),
                             p("We decided to work on figuring out the variables that can
                               influence the causation of heart disease because one of the members
                               has experience with heart disease from an early on. The variables we 
                               looked at were smoking, BMI, physical activity, and age. We stepped outside 
                               the box and looked at external factors like location within the USA (County) 
                               and economic class (every 25% of the population in terms of income). During research, 
                               we found out that white people were recorded way more than the other races."),
                             h3("Scope"),
                             p("
                               Internal variables: smoking, BMI, physical activity, and age
                              External variables: location (County in the USA), economic class (every 25% of the population in terms of income)

                               "),
                             h3("Requirements"),

                             p("
                               Combined table: heart disease, sex, age
                              Pivot tables: activity (BMI, physical activity, heart disease), substance (smoking, heart disease), state ( state, race/ethnicity)
                              Compare the proportions of each category and if they had a heart disease or not.
                              Making an interactive map showing the death rate and income of people within the USA

                               "),
                             h3("Backlog"),
                             p("
                               Correlation: did not get much information out of this, and did not work for two of the datasets
                               "),
                             
                             ),
                      ),
              ),
              tabPanel("Health Charts",
                       fluidRow(
                         column(6, plotOutput("ageSexChart")),
                         column(6, plotOutput("substanceChart"))
                       ),
                       fluidRow(
                         column(12, plotOutput("checkupChart"))
                       ),
                       fluidRow(
                         column(12, plotOutput("activityChart"))
                       )
              ),
              tabPanel("Race Charts",
                       fluidRow(
                         column(6, plotOutput("raceChart1")),
                         column(6, plotOutput("raceChart2"))
                       ),
                       fluidRow(
                         column(12, plotOutput("stateChart")),
                       )
              ),
              tabPanel("Map"),
              tabPanel("Predict Simple Model",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("genHealth", "General Health",
                                       choices = c("Excellent","Very good","Good","Fair","Poor")),
                           numericInput("physDays",   "Physical Health Days", min = 0, max = 30, value = 2),
                           selectInput("angina",      "Had Angina?",   choices = c("No","Yes")),
                           selectInput("stroke",      "Had Stroke?",   choices = c("No","Yes")),
                           selectInput("diabetes",    "Had Diabetes?", choices = c("No","Yes")),
                           numericInput("mhdays",     "Mental Health Days", min = 0, max = 30, value = 0),
                           numericInput("sleep",      "Sleep Hours",        min = 0, max = 24, value = 7),
                           actionButton("go", "Run Prediction")
                         )
                         ,
                         
                         mainPanel(
                           verbatimTextOutput("outText"),
                           plotOutput("outPlot")
                         )
                       )
              )
  ),
  
  conditionalPanel(
    "input.which_tab == 'Map'",
    fluidRow(
      column(3,
             selectInput("map_var", "Map variable:",
                         choices = c(
                           "Median Income"                  = "income",
                           "Heart-Disease Death Rate"       = "death_rate",
                           "Death-Rate per $1k Income"      = "death_income_ratio",
                           "Mean Death-Rate by Income Qtl." = "mean_death"
                         )
             )
      ),
      column(9,
             leafletOutput("usMap", height = "600px")
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$ageSexChart <- renderPlot({
    full_table %>%
      filter(!is.na(AgeCategory)) %>%
      ggplot(aes(x = AgeCategory, y = Count, fill = HeartDisease)) +
      geom_bar(stat = "identity") +
      facet_wrap(~Sex) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$raceChart1 <- renderPlot({
    ggplot(race_table, aes(x=Race, y=Count, fill=HeartDisease)) +
      geom_bar(stat="identity") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=45, hjust=1))
  })
  
  output$raceChart2 <- renderPlot({
    ggplot(race_table_2, aes(x=RaceEthnicityCategory, y=Count, fill=HeartDisease)) +
      geom_bar(stat="identity") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=45, hjust=1))
  })
  
  output$stateChart <- renderPlot({
    ggplot(state_table, aes(x=State, y=Count, fill=RaceEthnicityCategory)) +
      geom_bar(stat="identity") +
      coord_flip() +
      theme_minimal()
  })
  
  output$activityChart <- renderPlot({
    ggplot(heart_data_cleaned_20, aes(x = BMI, fill = HeartDisease)) +
      geom_histogram(binwidth = 1, boundary = 0, position = "stack") +
      labs(
        title =    "Heart Disease Cases by BMI",
        x =        "Body Mass Index (BMI)",
        y =        "Count of Patients",
        fill =     "Heart Disease Status"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(size = 16, face = "bold", hjust = 0.5)
      )
  })
  
  output$substanceChart <- renderPlot({
    ggplot(substance_table, aes(x=Smoking, y=Count, fill=HeartDisease)) +
      geom_bar(stat="identity", position="dodge") +
      theme_minimal()
  })
  
  output$checkupChart <- renderPlot({
  ggplot(Check_up_table, aes(x = State, y = Count, fill = LastCheckupTime)) +
    geom_bar(stat = "identity") +
    labs(
      title = "State Cases by Last Checkup Time",
      x     = "State",
      y     = "Count",
      fill  = "Last Checkup"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
    coord_flip()
})
  
  map_sf <- reactive({
    df <- counties_combined
    var <- input$map_var
    legend_title <- switch(var,
                           income               = "Median Income",
                           death_rate           = "Heart-Disease Death Rate",
                           death_income_ratio   = "Death-Rate per $1k Income",
                           mean_death           = "Mean Death-Rate by Income Quintile"
    )
    df %>% mutate(MapValue = .data[[var]]) -> df
    list(sf = df, legend = legend_title)
  })
  
  
  pal <- reactive({
    colorNumeric(
      palette  = myPurples,
      domain   = map_sf()$sf$MapValue,
      na.color = "transparent"
    )
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

  
  newdat <- eventReactive(input$go, {
    tibble(
      GeneralHealth      = factor(input$genHealth,  levels = c("Excellent","Very good","Good","Fair","Poor")),
      PhysicalHealthDays = input$physDays,
      HadAngina          = factor(input$angina,     levels = c("No","Yes")),
      HadStroke          = factor(input$stroke,     levels = c("No","Yes")),
      HadDiabetes        = factor(input$diabetes,   levels = c("No","Yes")),
      MentalHealthDays   = input$mhdays,
      SleepHours         = input$sleep
    )
  })
  
  
  result <- eventReactive(input$go, {
    nd   <- newdat()
    prob <- predict(model, nd,   type = "prob")$.pred_Yes
    cls  <- predict(model, nd,   type = "class")$.pred_class
    list(class = cls, prob = prob)
  })
  
  
  output$outText <- renderText({
    req(result())
    glue(
      "Predicted: {result()$class}\n",
      "P(HeartDisease='Yes') = {round(result()$prob, 3)}"
    )
  })
  
  output$outPlot <- renderPlot({
    req(result())
    barplot(
      c(No = 1 - result()$prob, Yes = result()$prob),
      beside = TRUE, ylim = c(0,1),
      ylab = "Probability", main = "Prediction Probabilities"
    )
  })
  
  
}

shinyApp(ui, server)