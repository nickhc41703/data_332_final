library(shiny)
library(dplyr)
library(plotly)
library(dplyr)
library(stringr)
library(ggplot2)
library(rpart)
library(gridExtra)

setwd('C:/Users/nicho/Documents/r_project/data/heart')

heart_data <- readRDS("heart.rds")
heart_data_cleaned_20 <- readRDS("heart_2020_cleaned.rds")
heart_data_no_nans_22 <- readRDS("heart_2022_no_nans.rds")


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

# Define the correct order of age categories
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Angles the labels

# Select relevant numeric columns for correlation for Heart_data
correlation_selection <- heart_data %>% select(`HeartDisease`, `RestingBP`, `Cholesterol`, `FastingBS`)

# Compute correlation matrix and convert to data frame for heart_data
cor_matrix <- cor(correlation_selection, use="complete.obs")
cor_df <- as.data.frame(as.table(cor_matrix))

#eh regression
regression <- glm(HeartDisease ~ Age + Cholesterol + RestingBP + MaxHR, data=heart_data, family=binomial)
summary(regression)

#useful tree model
heart_data_tree <- readRDS("heart.rds")
heart_data_tree$ChestPainType <- as.numeric(factor(heart_data_tree$ChestPainType))
heart_data_tree <- model.matrix(~ChestPainType + Sex + ExerciseAngina, data=heart_data_tree)
heart_data_tree$ChestPainType <- factor(heart_data_tree$ChestPainType, labels = c("ATA", "NAP", "ASY", "TA"))
heart_data_tree$Sex <- factor(heart_data_tree$Sex, labels = c("M", "F"))
heart_data_tree$ExerciseAngina <- factor(heart_data_tree$ExerciseAngina, labels = c("N", "Y"))

tree_model <- rpart(HeartDisease ~ ., data=heart_data_tree, method="class")
plot(tree_model)
text(tree_model)

#tree model for 2020
heart_data_tree_2020 <- readRDS("heart_2020_cleaned.rds")
tree_model <- rpart(HeartDisease ~ ., data=heart_data_tree_2020, method="class")
plot(tree_model)
text(tree_model)
#eh
#svm_model <- svm(HeartDisease ~ ., data=heart_data)
#summary(svm_model)

#heart_data_pca <- readRDS("heart.rds")
#heart_data_pca$Sex <- as.numeric(factor(heart_data_pca$Sex))
#heart_data_pca$ChestPainType <- as.numeric(factor(heart_data_pca$ChestPainType))
#heart_data_pca$RestingECG <- as.numeric(factor(heart_data_pca$RestingECG))
#heart_data_pca$ExerciseAngina <- as.numeric(factor(heart_data_pca$ExerciseAngina))
#heart_data_pca$ST_Slope <- as.numeric(factor(heart_data_pca$ST_Slope))

#pca <- prcomp(heart_data_pca[, -which(names(heart_data_pca) == "HeartDisease")], scale=TRUE)
#summary(pca)

# Select relevant numeric columns for correlation for 2020 table
#correlation_selection_2 <- heart_data_cleaned_20 %>% select(`HeartDisease`, `BMI`, `Smoking`, `AlcoholDrinking`)

# Compute correlation matrix and convert to data frame for 2020 table need work
#cor_matrix_2 <- cor(correlation_selection_2, use="complete.obs")
#cor_df_2 <- as.data.frame(as.table(cor_matrix_2))

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

# pivoting on heart disease and race for 2022
race_table_2 <- heart_data_no_nans_22 %>%
  group_by(RaceEthnicityCategory, HadHeartAttack) %>%
  summarise(Count = n(), .groups = "drop")

# Create the stacked bar chart for 2022
ggplot(race_table_2, aes(x = RaceEthnicityCategory, y = Count, fill = HadHeartAttack)) +
  geom_bar(stat = "identity") +
  labs(title = "Heart Disease Cases by Race",
       x = "Race",
       y = "Count",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# pivoting on state and race for 2022
state_table <- heart_data_no_nans_22 %>%
  group_by(RaceEthnicityCategory, State) %>%
  summarise(Count = n(), .groups = "drop")

# Create the stacked bar chart for 2022
ggplot(state_table, aes(x = State, y = Count, fill = RaceEthnicityCategory)) +
  geom_bar(stat = "identity") +
  labs(title = "State Cases by Race",
       x = "State",
       y = "Count",
       fill = "Race") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  coord_flip
#heart disease count for activity for 2020

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

