library(mlr)
library(caret)
library(glmnet)
library(dplyr)
library(ggplot2)

makeLearner("regr.lm")

source("Code/helpers.r")

# lasso regression for extra evidence

set.seed(123)

PROJECT_PATH <- ""
SEP <- ""
DATASET_PATH <- paste(PROJECT_PATH, "dataset/data_cleaned.csv", sep=SEP)

print("(1) Initial data analysis")
dataset <- fetch_data(DATASET_PATH)

# EDA -------------------
head(dataset)

display_na_statistics(dataset)
display_col_names(dataset)
summary(dataset)

print("(2) Performing data preparation")
print("Filtering out NA's...")
dataset <- fetch_na_filtered_data(dataset)
print("Observations count")
nrow(dataset)

print("Applyging log transform efficiency_wh_per_km")
dataset$log_efficiency_wh_per_km <- log(dataset$efficiency_wh_per_km)

print("Assigning car volume (mm)")
dataset$volume_mm <- get_car_volume_mm(dataset)

# Dropping not needed variables. Matters for interpretation
dataset <- dataset %>% select(-segment)
dataset <- dataset %>% select(-brand)
dataset <- dataset %>% select(-top_speed_kmh)
dataset <- dataset %>% select(-drivetrain)
dataset <- dataset %>% select(-car_body_type)
dataset <- dataset %>% select(-log_torque_nm)
dataset <- dataset %>% select(-log_top_speed_kmh)
dataset <- dataset %>% select(-log_fast_charging_power_kw_dc)
dataset <- dataset %>% select(-log_cargo_volume_l)
dataset <- dataset %>% select(-log_acceleration_0_100_s)
dataset <- dataset %>% select(-efficiency_wh_per_km)
dataset <- dataset %>% select(-height_mm)
dataset <- dataset %>% select(-width_mm)
dataset <- dataset %>% select(-length_mm)
dataset <- dataset %>% select(-range_km)
dataset <- dataset %>% select(-cargo_volume_l)



x <- model.matrix(log_efficiency_wh_per_km ~ ., data=dataset)[, -1]
y <- log(dataset$log_efficiency_wh_per_km)

# if alpha = 1, L1 norm applied - lasso regression
# standardize is used due to different scale of predictors

print("Performing cross validation")
lasso_cv_fit <- cv.glmnet(x, y, alpha = 1, standardize = TRUE)
summary(lasso_cv_fit)

print("Performing final model fit")
lasso_model <- glmnet(
  x, 
  y, 
  alpha = 1, 
  lambda = lasso_cv_fit$lambda.min, 
  standardize = TRUE
)

betas <- coef(lasso_model)
betas

print("Plotting variable importance")
coef_df <- data.frame(
  variable = rownames(betas),
  coefficient = as.numeric(betas)
)

# Remove intercept and zero coefficients
coef_df <- subset(coef_df, coefficient != 0 & variable != "(Intercept)")

# Create importance metric
coef_df$importance <- abs(coef_df$coefficient)


png("Code/mlr_lasso_bar_plot.png")
ggplot(coef_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  geom_col(fill = "darkgreen") +
  labs(title = "LASSO Variable Importance for MLR",
       x = "Variables",
       y = "Variable Importance") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12)
  )

print("DONE")
