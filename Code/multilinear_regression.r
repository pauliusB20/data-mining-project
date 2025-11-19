library(ggplot2)
library(gridExtra)
library(MASS)
library(car)

# y - efficiency_wh_per_km
# x - pick by variable selection
PLOT_WIDTH <- 1024
PLOT_HEIGHT <- 1500

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

# TODO: fix sink
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

create_result_plots <- function(dataset, predictor_names) {
    plots <- list()
    plot_aes <- aes(
        y = efficiency_wh_per_km,
    )
    for (i in 1:length(predictor_names)) {
        predictor_name <- predictor_names[[i]]
        plot_aes <- modifyList(
            plot_aes,
            aes(x = !!sym(predictor_name))
        )
        # dataset initial plot
        plots <- append(
            plots, 
            ggplot(
                dataset, 
                plot_aes
            ) + 
            geom_point() +
            geom_smooth(method = "lm") +
            xlab(predictor_name) +
            ylab("Efficiency WH/km (Original)") 
        )

        # predicted data y_hat
        plots <- append(
            plots, 
            ggplot(
                dataset, 
                aes(
                    x = !!sym(predictor_name),
                    y = dataset$y_hat
                )
            ) + 
            geom_point() +
            geom_smooth(method = "lm") +
            xlab(predictor_name) +
            ylab("Efficiency WH/km (Predicted)") 
        )
    }

    do.call(
        grid.arrange, 
        c(
            plots, 
            ncol = 2,
            top = paste(
                predictor_name, 
                " and Efficiency WH/km regression"
            )
        )
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
predictors <- c(
    "battery_capacity_kWh",
    "width_mm",
    "height_mm", 
    "length_mm",
    "torque_nm",
    "top_speed_kmh"
)

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
    torque_nm +
    top_speed_kmh, 
    data = dataset_train
)
summary(mlr_model)

print("Showing multicollinearity check results")
vif(mlr_model)


print("Training...")
dataset_train_pred <- data.frame(response=predict(mlr_model, dataset_train))
y_train <- dataset_train$efficiency_wh_per_km
y_hat_train <- dataset_train_pred$response
display_metric_results("Training metric", y_train, y_hat_train)

dataset_test_pred <- data.frame(response=predict(mlr_model, dataset_test))
y_test <- dataset_test$efficiency_wh_per_km
y_hat_test <- dataset_test_pred$response

display_metric_results("Test metric", y_test, y_hat_test)


print("Performing final model building")
dataset_pred <- data.frame(response=predict(mlr_model, dataset))
dataset["y_hat"] <- dataset_pred$response
print("Printing results")

display_metric_results(
    "Final Model", 
    dataset$efficiency_wh_per_km, 
    dataset$y_hat
)

png(
    "Code/mlr_results.png", 
    height = PLOT_HEIGHT,
    width = PLOT_WIDTH
) 

# TODO: to combine in PNG
create_result_plots(dataset, predictors)
error_residuals <- residuals(mlr_model)

png("Code/mlr_results_qq_error.png")
print("Printing Q-Q plot")
qqnorm(error_residuals)   # Plot sample quantiles
qqline(error_residuals)   # Add reference line

print("Checking residual normality using test")
shapiro.test(error_residuals)

print("DONE")