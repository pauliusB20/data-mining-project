library(ggplot2)
library(gridExtra)
library(MASS)
library(car)
library(pls)

# y - efficiency_wh_per_km
# x - pick by variable selection

fetch_data <- function() {
    return (read.csv("dataset/data_cleaned.csv"))
}

display_na_statistics <- function(data) {
    print("Displaying NA statistics")
    colSums(is.na(data))
}

display_col_names <- function(data) {
    print("Displaying collumn names")
    colnames(data)
}

na_to_zero <- function (data) {
    data[is.na(data)] <- 0
    return (data)
}

display_metric_results <- function(result_type, y, y_hat) {
    rss <- sum((y_hat - y)^2)
    mse <- mean((y_hat - y)^2)
    r2 <- 1 - rss / sum((y - mean(y))^2)

    print(paste(result_type, " results: "))
    print(
        paste(
            "RSS: ", rss,
            "MSE: ", mse,
            "R2: ", r2
        ),
    )
}

# TODO
# main <- function() {    
   
# }

dataset <- fetch_data()

print("Starting multilinear regression analysis...")

# EDA -------------------
head(dataset)

display_na_statistics(dataset)
display_col_names(dataset)
set.seed(123)

# checking normaliy
print("Checking Y variable normality")
sample_index <- sample(nrow(dataset), size = 60)     
sampled_data <- dataset[sample_index, ]
shapiro.test(sampled_data$efficiency_wh_per_km)

print("Nullifying NA's...")
dataset <- na_to_zero(dataset)

# -------------------------
print("Running linear regression")
# 1. pick predictors:
# battery_capacity, height_mm, length_mm

# analyzing model summary

print("Performing Train/Test split")
idx <- sample(
    1:nrow(dataset), 
    size=0.8 * nrow(dataset), 
    replace=TRUE
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
# using training data
# for analysis
mlr_model <- lm(
    efficiency_wh_per_km ~ 
    battery_capacity_kWh + 
    width_mm +
    height_mm + 
    length_mm +
    range_km +
    torque_nm, 
    data = dataset_train
)
summary(mlr_model)

print("Showing multicollinearity check results")
vif(mlr_model)

pcr_model <- pcr(
    efficiency_wh_per_km ~ 
    battery_capacity_kWh + 
    width_mm +
    height_mm + 
    length_mm +
    range_km +
    torque_nm,
    data = dataset_train, 
    scale = TRUE, 
    validation = "CV"
)

summary(pcr_model)


y_train <- dataset_train$efficiency_wh_per_km
y_hat_train <- predict(pcr_model, dataset_train)
display_metric_results("Training metric", y_train, y_hat_train)

print("DONE")