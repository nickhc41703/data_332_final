# train_model_simple.R

library(tidymodels)

# 1. Load & rename
df <- readRDS("DataClean/heart_2022_no_nans.rds") %>%
  rename(HeartDisease = HadHeartAttack) %>%
  mutate(
    HeartDisease      = factor(HeartDisease,      levels = c("No","Yes")),
    GeneralHealth     = factor(GeneralHealth,     levels = c("Excellent","Very good","Good","Fair","Poor")),
    HadAngina         = factor(HadAngina,         levels = c("No","Yes")),
    HadStroke         = factor(HadStroke,         levels = c("No","Yes")),
    HadDiabetes       = factor(HadDiabetes,       levels = c("No","Yes"))
  )

# 2. Train/test split
splits <- initial_split(df, prop = 0.8, strata = HeartDisease)
train  <- training(splits)
test   <- testing(splits)

# 3. Recipe on three real columns
rec_simple <- recipe(HeartDisease ~ PhysicalHealthDays + MentalHealthDays + SleepHours,
                     data = train) %>%
  step_normalize(all_numeric_predictors())

# 4. Model spec
log_mod <- logistic_reg() %>% set_engine("glm")

# 5. Workflow
wf_simple <- workflow() %>%
  add_model(log_mod) %>%
  add_recipe(rec_simple)

# 6. Fit
fit_simple <- fit(wf_simple, data = train)

# 7. Optional evaluation
preds <- predict(fit_simple, test, type = "prob") %>%
  bind_cols(test)
roc_auc(preds, truth = HeartDisease, .pred_Yes) %>% print()

# 8. Save
dir.create("models", showWarnings = FALSE)
saveRDS(fit_simple, "DataClean/heart_model_simple.rds")
