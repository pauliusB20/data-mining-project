library(ggplot2)
library(gridExtra)
library(MASS)
library(car)
library(quantreg)
library(qgam)
library(caret)

# y - efficiency_wh_per_km
# x - pick by variable selection
# TODO: RMSE metric is missing
# TODO: box cox transformation
# TODO: log y dependabale (efficieny)
# TODO: set.seed(123)
# TODO: prideti hyperparametrus del palyginimu
# TODO: nenaudoti neiformatyviu duomenu
# TODO: pritaikyti cook distance
# TODO: pameginti quantile regression (palyginti)
# TODO: var importance metrika, kuri nustato kurie parametrai svarbus
# TODO: accelation variable

PLOT_WIDTH <- 1024
PLOT_HEIGHT <- 1500
PROJECT_PATH <- "practical_part/data-mining-project"
DATASET_PATH <- paste(PROJECT_PATH, "dataset/data_cleaned.csv", sep="/")
SCATTER_PLOT_PATH <- paste(PROJECT_PATH, "Code/mlr_scatter_plots.png", sep="/")
QQ_SAVE_PATH <- paste(PROJECT_PATH, "Code/mlr_results_qq_error.png", sep="/")
QUANTILE_REG_SAVE_PATH <- paste(PROJECT_PATH, "Code/qr_regression_results.png", sep="/")
COOK_DISTANCE_PATH <- paste(PROJECT_PATH, "Code/mlr_cook_distance.png", sep="/")

fetch_data <- function(path) {
    return (read.csv(path))
}

display_na_statistics <- function(data) {
    print("Displaying NA statistics")
    colSums(is.na(data))
}

display_col_names <- function(data) {
    print("Displaying collumn names")
    colnames(data)
}

get_car_volume_mm <- function(data) {
    volume = as.numeric(data$width_mm) * as.numeric(data$height_mm) * as.numeric(data$length_mm)
    return (volume)
}

fetch_na_filtered_data <- function(data) {
    return (na.omit(data))
}

# TODO: fix sink
display_metric_results <- function(result_type, y, y_hat) {
    rss <- sum((y_hat - y)^2)
    mse <- mean((y_hat - y)^2)
    rmse <- sqrt(mse)
    r2 <- 1 - rss / sum((y - mean(y))^2)

    print(paste(result_type, " results: "))
    print(
        paste(
            "RSS: ", rss,
            "MSE: ", mse,
            "RMSE: ", rmse,
            "R2: ", r2
        ),
    )
}

display_na_statistics <- function(data) {
    print("Displaying NA statistics")
    colSums(is.na(data))
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
            ylab("log(Efficiency WH/km) (Original)") 
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
            ylab("log(Efficiency WH/km) (Predicted)") 
        )
    }

    do.call(
        grid.arrange, 
        c(
            plots, 
            ncol = 2,
            top = paste(
                predictor_name, 
                " and log(Efficiency WH/km) regression"
            )
        )
    )
}


set.seed(123)
print("Starting multilinear regression analysis...")

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

png("log_efficiency_mlr.png")
hist(dataset$log_efficiency_wh_per_km)

print("Assigning car volume (mm)")
dataset$volume_mm <- get_car_volume_mm(dataset)

display_na_statistics(dataset)

# stop("Stopping")

print("(3) Doing secondary data analysis")
# checking normaliy
print("Checking Y variable normality")
sample_index <- sample(nrow(dataset), size = 60)     
sampled_data <- dataset[sample_index, ]
shapiro.test(sampled_data$efficiency_wh_per_km)

# -------------------------
print("(4) Running linear regression")
# 1. pick predictors:
# battery_capacity, height_mm, length_mm
# predictors <- c(
#     "battery_capacity_kWh",
#     "width_mm",
#     "height_mm", 
#     "length_mm",
#     "torque_nm",
#     "top_speed_kmh"
# )
predictors <- c(
    "volume_mm",
    "log_acceleration_0_100_s"
    # "log_torque_nm"
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

mlr_model <- lm(
    log_efficiency_wh_per_km ~ 
    volume_mm +
    log_acceleration_0_100_s,
    data = dataset_train
)
summary(mlr_model)


print("Showing multicollinearity check results")
# Variance Inflation Factors
vif(mlr_model)

print("Training...")
dataset_train_pred <- data.frame(response=predict(mlr_model, dataset_train))
y_train <- dataset_train$log_efficiency_wh_per_km
y_hat_train <- dataset_train_pred$response
display_metric_results("Training metric", y_train, y_hat_train)

dataset_test_pred <- data.frame(response=predict(mlr_model, dataset_test))
y_test <- dataset_test$log_efficiency_wh_per_km
y_hat_test <- dataset_test_pred$response


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
    SCATTER_PLOT_PATH, 
    height = PLOT_HEIGHT,
    width = PLOT_WIDTH
) 

# TODO: to combine in PNG
create_result_plots(dataset, predictors)
error_residuals <- residuals(mlr_model)

png(QQ_SAVE_PATH)
print("Printing Q-Q plot")
qqnorm(error_residuals)   # Plot sample quantiles
qqline(error_residuals)   # Add reference line

print("Checking residual normality using test")
shapiro.test(error_residuals)

print("Performing anova analysis")
mlr_model_null <- lm(log_efficiency_wh_per_km ~ 1, dataset_train)
anova(mlr_model_null, mlr_model)

# Result: model 2 (main mlr model) performs well with smaller dataset
print("Performing quantile regression")

qr_model <- qgam(
    log_efficiency_wh_per_km ~ volume_mm + log_acceleration_0_100_s, 
    data=dataset_train, 
    qu = 0.7
)
# Result: all 0 and significant p-value < 0.05
summary(qr_model, se="boot")

png(QUANTILE_REG_SAVE_PATH)
p1 <- ggplot(
    dataset_train, 
    aes(volume_mm,log_efficiency_wh_per_km)) + 
    geom_point() +
    geom_abline(intercept=coef(qr_model)[1], slope=coef(qr_model)[2])

p2 <- ggplot(
    dataset_train, 
    aes(log_acceleration_0_100_s,log_efficiency_wh_per_km)) + 
    geom_point() +
    geom_abline(intercept=coef(qr_model)[1], slope=coef(qr_model)[2])

grid.arrange(p1, p2)

print("Calcualting MLR Cook distance")

# Indicates
# data entry error

# true unusual observation

# leverage point (extreme X value)

# outlier in Y

# strong influence on model coefficients

png(COOK_DISTANCE_PATH)
cook_vals <- cooks.distance(mlr_model)
plot(cook_vals, type = "h", main = "Cook's Distance")


print("Doing automatic feature selection")


control <- rfeControl(functions = lmFuncs, method = "cv")
results <- rfe(dataset_train[, predictors], dataset_train$log_efficiency_wh_per_km,
               sizes = c(1:20),
               rfeControl = control)
# result: adding more  usefull variables, model explains more usefull information
results


print("DONE")