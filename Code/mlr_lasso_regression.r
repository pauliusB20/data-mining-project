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
lasso_model <- cv.glmnet(x, y, alpha = 1, standardize = TRUE)

betas <- coef(lasso_model)
betas

coef_df <- data.frame(
  predictor = row.names(as.matrix(betas)),
  coefficient = as.numeric(betas)
)
coef_df <- coef_df[coef_df$predictor != "(Intercept)", ]



coef_min <- coef(lasso_model, s = "lambda.min")
coef_min <- as.matrix(coef_min)
coef_min <- coef_min[coef_min != 0, , drop = FALSE]      # keep non-zero
coef_min <- coef_min[rownames(coef_min) != "(Intercept)", , drop = FALSE]
vals <- abs(coef_min[, 1])
names(vals) <- rownames(coef_min)
vals <- sort(vals, decreasing = TRUE)


png("Code/mlr_lasso_bar_plot.png", height=600)
par(mar = c(5, 15, 4, 2))
barplot(
  vals,
  horiz = TRUE,                 # flip the axes
  las = 1,                      # make y-labels readable
  xlab = "Variable Strength",
  ylab = "",
  main = "LASSO Predictor Strength",
  col = "purple",
  space = 3,
  cex.names = 1.1
)

