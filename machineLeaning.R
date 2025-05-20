library(tidymodels)
library(dplyr)

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
