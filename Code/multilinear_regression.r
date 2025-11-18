library(ggplot2)
library(gridExtra)
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
col_names <- c("battery_capacity_kWh", "height_mm", "length_mm")
set.seed(123)

# plotting dataset for multilinear regression
# ggplot(dataset, aes(x = battery_capacity_kWh, y = efficiency_wh_per_km)) + geom_point() + geom_smooth(method = "lm")
# ggplot(dataset, aes(x = height_mm, y = efficiency_wh_per_km)) + geom_point() + geom_smooth(method = "lm")
# ggplot(dataset, aes(x = length_mm, y = efficiency_wh_per_km)) + geom_point() + geom_smooth(method = "lm")

# -------------------------
print("Running linear regression")
# 1. pick predictors:
# battery_capacity, height_mm, length_mm

# analyzing model summary
model <- lm(
    efficiency_wh_per_km ~ 
    battery_capacity_kWh + 
    height_mm + 
    length_mm, 
    data = dataset
)
summary(model)


print("Train/Test split")
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
fit <- lm(
    efficiency_wh_per_km ~ 
    battery_capacity_kWh + 
    height_mm + 
    length_mm, 
    data = dataset_train
)

dataset_train_pred <- data.frame(response=predict(fit, dataset_train))
dataset_test_pred <- data.frame(response=predict(fit, dataset_test))

y_train <- dataset_train$efficiency_wh_per_km
y_hat_train <- dataset_train_pred$response

y_test <- dataset_test$efficiency_wh_per_km
y_hat_test <- dataset_test_pred$response

print("Training set evaluation:")

display_metric_results("Training Data", y_train, y_hat_train)

print("Test set evaluation:")
display_metric_results("Test Data", y_test, y_hat_train)

print("Performing final model building")
dataset_pred <- data.frame(response=predict(fit, dataset))
dataset["y_hat"] <- dataset_pred$response

print("Printing results")
pdf("Code/mlr_results_1.pdf")
p1 <- ggplot(
    dataset, 
    aes(
        x = battery_capacity_kWh, 
        y = efficiency_wh_per_km
    )) + 
    geom_point() +
    geom_smooth(method = "lm") +
    xlab("Battery Capacity") +
    ylab("Efficiency WH/km (Original)")


p2 <- ggplot(
    dataset, 
    aes(
        x = battery_capacity_kWh, 
        y = y_hat
    )) + 
    geom_point() +
    geom_smooth(method = "lm") +
    xlab("Battery Capacity") +
    ylab("Efficiency WH/km (Prediction)")
    

grid.arrange(
    p1, 
    p2, 
    ncol = 2, 
    top = "Battery Capacity and Efficiency WH/km regression"
)

print("DONE")