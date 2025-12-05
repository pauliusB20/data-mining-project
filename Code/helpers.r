library(ggplot2)
library(gridExtra)
library(car)
library(caret)

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

display_metric_results <- function(result_type, y, y_hat) {
    mae <- mean(abs(y_hat - y))
    rss <- sum((y_hat - y)^2)
    mse <- mean((y_hat - y)^2)
    rmse <- sqrt(mse)
    r2 <- 1 - rss / sum((y - mean(y))^2)

    print(paste(result_type, " results: "))
    print(
        paste(
            "RSS: ", rss,
            "MAE: ", mae, 
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

create_result_plots <- function(project_dir, dataset, predictor_names, log_used, height, width) {
    plot_aes <- aes(
        y = efficiency_wh_per_km,
    )
    y_label_log <- "log(Efficiency WH/km)"
    y_label_nolog <- "Efficiency WH/km"

    for (i in 1:length(predictor_names)) {
        predictor_name <- predictor_names[[i]]
        suffix <- ""
        using_y_label <- y_label_nolog
        if (log_used == TRUE) {
            suffix <- "_log"
            using_y_label <- y_label_log
        }
        plot_file_name <- paste(predictor_name, "_efficiency_result", suffix, ".png", sep="")

        # creating plot
        png(paste(project_dir, plot_file_name, sep="/"))

        plot_aes <- modifyList(
            plot_aes,
            aes(x = !!sym(predictor_name))
        )
        # dataset initial plot
        p1 <- ggplot(
                dataset, 
                plot_aes
            ) + 
            geom_point() +
            geom_smooth(method = "lm") +
            xlab(predictor_name) +
            ylab(using_y_label) 

        # predicted data y_hat
        p2 <- ggplot(
                dataset, 
                aes(
                    x = !!sym(predictor_name),
                    y = dataset$y_hat
                )
            ) + 
            geom_point() +
            geom_smooth(method = "lm") +
            xlab(predictor_name) +
            ylab(paste(using_y_label, " (Predicted)")) 

        grid.arrange(
            p1, p2, ncol = 1, nrow = 2
        )
    }

}


