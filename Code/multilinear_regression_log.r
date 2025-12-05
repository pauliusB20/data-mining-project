library(ggplot2)
library(gridExtra)
library(MASS)
library(car)
library(quantreg)
library(qgam)
library(caret)


PLOT_WIDTH <- 1024
PLOT_HEIGHT <- 1500
PROJECT_PATH <- ""
SEP <- ""
DATASET_PATH <- paste(PROJECT_PATH, "dataset/data_cleaned.csv", sep=SEP)
SCATTER_PLOT_PATH <- paste(PROJECT_PATH, "Code/mlr_scatter_plots_log.png", sep=SEP)
QQ_SAVE_PATH <- paste(PROJECT_PATH, "Code/mlr_results_qq_error_log.png", sep=SEP)
COOK_DISTANCE_PATH <- paste(PROJECT_PATH, "Code/mlr_cook_distance_log.png", sep=SEP)
ERROR_VAR_PLOT_PATH <- paste(PROJECT_PATH, "Code/mlr_homoscadacity_error_var.png", sep=SEP)

# importing helper methods 
source("Code/helpers.r")

set.seed(123)
print("Starting multilinear regression analysis...")

print("(1) Initial data analysis")
dataset <- fetch_data(DATASET_PATH)

# EDA -------------------
head(dataset)

display_na_statistics(dataset)
display_col_names(dataset)


print("(2) Performing data preparation")
print("Filtering out NA's...")
dataset <- fetch_na_filtered_data(dataset)
print("Observations count")
nrow(dataset)

print("Applyging log transform efficiency_wh_per_km")
dataset$log_efficiency_wh_per_km <- log(dataset$efficiency_wh_per_km)

print("Assigning car volume (mm)")
dataset$volume_mm <- get_car_volume_mm(dataset)

summary(dataset)

print("(3) Doing secondary data analysis")

# checking normaliy
print("Checking Y variable normality log_efficiency_wh_per_km")
sample_index <- sample(nrow(dataset), size = 60)     
sampled_data <- dataset[sample_index, ]
shapiro.test(sampled_data$log_efficiency_wh_per_km)

print("Displaying Y variable distirbution histogram")
png("Code/log_efficiency_mlr.png")
hist(dataset$log_efficiency_wh_per_km)

display_na_statistics(dataset)


print("(4) Performing multi-linear regression")

predictors <- c(
    "torque_nm",
    "acceleration_0_100_s",
    "battery_capacity_kWh",
    "acceleration_0_100_s",
    "volume_mm"
)

# analyzing model summary

print("Performing Train/Test split")
idx <- sample(
    1:nrow(dataset), 
    size=0.8 * nrow(dataset), 
    replace=FALSE
)

dataset_train <- dataset[idx,]
dataset_test <- dataset[-idx,]

print(
    paste(
        "Train data size: ", 
        nrow(dataset_train),
        "Test data size: ", 
        nrow(dataset_test)
    )
)

print("Performing MLR model training")

mlr_model <- lm(
  log_efficiency_wh_per_km ~
    torque_nm +
    acceleration_0_100_s +
    battery_capacity_kWh +
    volume_mm,
    data = dataset_train
)
summary(mlr_model)


print("Showing multicollinearity check results")
# Variance Inflation Factors
vif(mlr_model)

# Results: No severe multicolinearity. All predictors VIF < 5. 
# Indicates no severe multicolinearity. 

print("Training the model...")
dataset_train_pred <- data.frame(response=predict(mlr_model, dataset_train))
y_train <- dataset_train$log_efficiency_wh_per_km
y_hat_train <- dataset_train_pred$response
display_metric_results("Training metric", y_train, y_hat_train)

# NOTE: check only MSE/RMSE for test set
print("Testing the model with unseen data")
dataset_test_pred <- data.frame(response=predict(mlr_model, dataset_test))
y_test <- dataset_test$log_efficiency_wh_per_km
y_hat_test <- dataset_test_pred$response
display_metric_results("Test metric", y_test, y_hat_test)


print("(5) Performing final model building")
mlr_model_final <- lm(
    log_efficiency_wh_per_km ~
    torque_nm +
    acceleration_0_100_s +
    volume_mm,
    data = dataset
)
dataset_pred <- data.frame(response=predict(mlr_model_final, dataset))
dataset["y_hat"] <- dataset_pred$response
print("Printing results")

display_metric_results(
    "Final Model", 
    dataset$log_efficiency_wh_per_km, 
    dataset$y_hat
)

print("Multi-linear regression model coeficients")
coef(mlr_model_final)

png(
    SCATTER_PLOT_PATH, 
    height = PLOT_HEIGHT,
    width = PLOT_WIDTH
) 


create_result_plots("Code", dataset, predictors, TRUE, PLOT_HEIGHT, PLOT_WIDTH)
error_residuals <- residuals(mlr_model)

png(QQ_SAVE_PATH)
print("Printing Q-Q plot for evaluating model residuals")
qqnorm(error_residuals)   # Plot sample quantiles
qqline(error_residuals)   # Add reference line

print("Displaying plot for homoscadacity assumption check")
png(ERROR_VAR_PLOT_PATH)
plot(mlr_model_final, which = 1)


print("Performing anova analysis")
mlr_model_null <- lm(log_efficiency_wh_per_km ~ 1, dataset_train)
anova(mlr_model_null, mlr_model)


print("Calculating MLR Cook distance")

png(COOK_DISTANCE_PATH)
cook_vals <- cooks.distance(mlr_model)
plot(cook_vals, type = "h", main = "Cook's Distance")

print("DONE")


