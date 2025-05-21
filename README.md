READ ME

# Heart Disease Influences

## By Nick Camacho and Tommy Anderson

## Introduction
We decided to work on figuring out the variables that can influence the causation of heart disease because one of the members has experience with heart disease from an early on. The variables we looked at were smoking, BMI, physical activity, and age. We stepped outside the box and looked at external factors like location within the USA (County) and economic class (every 25% of the population in terms of income). During research, we found out that white people were recorded way more than the other races.


INTERACT WITH OUR SHINY APP [HERE]([shiny app](https://tommyanderson.shinyapps.io/Heart_Disease_Shiny/))!


## Datasets
-We found three datasets on Kaggle. We were only able to find similarities in the columns that said if they had a heart disease or not, their gender, and their age

## Scope
-Internal variables: smoking, BMI, physical activity, and age
External variables: location (County in the USA), economic class (every 25% of the population in terms of income)

## Requirements
-Combined table: heart disease, sex, age
Pivot tables: activity (BMI, physical activity, heart disease), substance (smoking, heart disease), state ( state, race/ethnicity)
Compare the proportions of each category and whether they had heart disease or not.
Making an interactive map showing the death rate and income of people within the USA

## Key Findings
Map: In the maps, we were able to find out that the southern areas usually lower income usually have a higher mortality rate from heart disease. This is shown in the 2 maps with deaths overlayed with income and the split on the 25 percentiles.

Gender/Age table: Men typically have a greater chance of having heart disease when they are 70-74 years old, and women have a greater chance when they are 80 years old or older.
Activity Table: Being physically active reduces the probability of having heart disease
Substance table: smoking increases the chances of having heart disease
We noticed that the difference between POC and White people who went to the hospital was drastic. 

```
# Rename ChestPainType column and transform values
heart_data <- heart_data %>%
  rename(HadAngina = ChestPainType) %>%
  mutate(HadAngina = ifelse(HadAngina %in% c("TA", "ASY", "ATA"), "Yes", "No"))

heart_data <- heart_data %>%
  mutate(Sex = recode(Sex, "M" = "Male", "F" = "Female"))

# Create 5-year age categories and name the new column "AgeCategory"
age_min <- floor(min(heart_data$Age, na.rm = TRUE) / 5) * 5
age_max <- ceiling(max(heart_data$Age, na.rm = TRUE) / 5) * 5
breaks <- seq(age_min, age_max, by = 5)

# Create age category labels like "60-64"
labels <- paste(head(breaks, -1), breaks[-1] - 1, sep = "-")

# Apply the cut function
heart_data$AgeCategory <- cut(heart_data$Age, breaks = breaks, labels = labels, right = FALSE)

heart_data <- heart_data[, !names(heart_data) %in% "Age"]

#pivot for age category, sex, heart_disease - heart data
general_table_1 <- heart_data %>%
  group_by(AgeCategory, Sex, HeartDisease) %>%
  summarise(Count = n(), .groups = "drop")

#changes from 0 to no and 1 to yes
general_table_1 <- general_table_1 %>%
  mutate(HeartDisease = ifelse(HeartDisease == 1, "Yes", "No"))

#pivot for age category, sex, heart_disease - heart_data_cleaned_20
general_table_2 <- heart_data_cleaned_20 %>%
  group_by(AgeCategory, Sex, HeartDisease) %>%
  summarise(Count = n(), .groups = "drop")

general_table_2 <- general_table_2 %>%
  mutate(AgeCategory = str_replace_all(AgeCategory, c(" or older" = "+")))

#pivot for age category, sex, heart_disease - heart_data_no_nans_22
general_table_3 <- heart_data_no_nans_22 %>%
  group_by(AgeCategory, Sex, HadHeartAttack) %>%
  summarise(Count = n(), .groups = "drop")

#change column name
colnames(general_table_3)[colnames(general_table_3) == "HadHeartAttack"] <- "HeartDisease"

# Modify AgeCategory column
general_table_3 <- general_table_3 %>%
  mutate(AgeCategory = str_replace_all(AgeCategory, c("Age " = "", " to " = "-", " or older" = "+")))

#joining data
full_table <- rbind(general_table_1, general_table_2, general_table_3)

```
This code was used to transfomr the three tables so that are compatible and bind them together

```
age_order <- c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
               "55-59", "60-64", "65-69", "70-74", "75-79", "80+", "80 or older")

# Convert AgeCategory to a factor with the defined order
full_table$AgeCategory <- factor(full_table$AgeCategory, levels = age_order)

ggplot(full_table, aes(x = AgeCategory, y = Count, fill = HeartDisease)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Sex) +
  labs(title = "Stacked Bar Chart of Heart Disease by Age Category and Sex",
       x = "Age Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```
This was used to make the age/gender bar graph
```
# pivoting on heart disease and race
race_table <- heart_data_cleaned_20 %>%
  group_by(Race, HeartDisease) %>%
  summarise(Count = n(), .groups = "drop")

# Create the stacked bar chart
ggplot(race_table, aes(x = Race, y = Count, fill = HeartDisease)) +
  geom_bar(stat = "identity") +
  labs(title = "Heart Disease Cases by Race",
       x = "Race",
       y = "Count",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```
This was used to make one of the pivot/bar graphs for cases by race
```
Activity_table <- heart_data_cleaned_20 %>%
  group_by(HeartDisease, BMI, PhysicalActivity) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(Activity_table, aes(x = BMI, y = Count, fill = HeartDisease)) +
  geom_bar(stat = "identity") +
  labs(title = "Heart Disease Cases by Bmi",
       x = "BMI",
       y = "Count",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```
This was used to pivot/make the graph by BMI
```
#heart disease count for substance for 2020
substance_table <- heart_data_cleaned_20 %>%
  group_by(HeartDisease, Smoking, AlcoholDrinking) %>%
  summarise(Count = n(), .groups = "drop")

# Create smoking-related plot
ggplot(substance_table, aes(x=Smoking, y=Count, fill=HeartDisease)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Heart Disease by Smoking Status",
       x="Smoking Status", y="Count") +
  theme_minimal()
```
This was used to pivot/ make the graph for smoking
```
#last checkup time
Check_up_table <- heart_data_no_nans_22 %>%
  group_by(HadHeartAttack, State, LastCheckupTime) %>%
  summarise(Count = n(), .groups = "drop")

colnames(Check_up_table)[colnames(Check_up_table) == "HadHeartAttack"] <- "HeartDisease"

ggplot(Check_up_table, aes(x = State, y = Count, fill = LastCheckupTime)) +
  geom_bar(stat = "identity") +
  labs(title = "State Cases by LastCheckupTime",
       x = "State",
       y = "Count",
       fill = "LastCheckupTime") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  coord_flip()
```
This was used to get the last checkup by state and make a graph of it.
```
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
```
This code was used to make the interactive map and display the information by counties. we made it display information on median income, heart-disease death rate, death rate per $1k income, and mean death rate by income by every 25%
```
# 2. Read & rename
df <- readRDS("DataClean/heart_2022_no_nans.rds") %>%
  rename(HeartDisease = HadHeartAttack) %>%
  mutate(
    HeartDisease      = factor(HeartDisease,      levels = c("No","Yes")),
    GeneralHealth     = factor(GeneralHealth,     levels = c("Excellent","Very good","Good","Fair","Poor")),
    HadAngina         = factor(HadAngina,         levels = c("No","Yes")),
    HadStroke         = factor(HadStroke,         levels = c("No","Yes")),
    HadDiabetes       = factor(HadDiabetes,       levels = c("No","Yes"))
  )

splits <- initial_split(df, prop = 0.8, strata = HeartDisease)
train  <- training(splits)
test   <- testing(splits)

# 4. Recipe on the 7 predictors (no dummy for numeric; factors are left as-is)
rec_simple <- recipe(
  HeartDisease ~ GeneralHealth +
    PhysicalHealthDays +
    HadAngina +
    HadStroke +
    HadDiabetes +
    MentalHealthDays +
    SleepHours,
  data = train
) %>%
  step_normalize(all_numeric_predictors())

# 5. Model spec & workflow
wf_simple <- workflow() %>%
  add_model(logistic_reg() %>% set_engine("glm")) %>%
  add_recipe(rec_simple)

# 6. Fit
fit_simple <- fit(wf_simple, data = train)

# 7. Quick check
preds <- predict(fit_simple, test, type = "prob") %>%
  bind_cols(test)
print( roc_auc(preds, truth = HeartDisease, .pred_Yes) )

# 8. Save (overwrite the old simple model)
dir.create("models", showWarnings = FALSE)
saveRDS(fit_simple, "DataClean/heart_model_simple.rds")
```
This code was used to make our LLM model so that we can put in change information based on five variables and see the percentage of the likelihood of having heart disease
