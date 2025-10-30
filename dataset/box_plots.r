library("gridExtra")
library("ggplot2")

input_data <- read.csv("electric_vehicles_spec_2025.csv.csv")

print("Creating box plot by electric vehicle type")
png("box_plot_by_type.png", width = 1800, height = 1500, res = 280)


ggplot(input_data, aes(x = efficiency_wh_per_km, y = car_body_type, fill = car_body_type)) +
  geom_boxplot(alpha=0.2) +
  labs(
    title = "Box plots by electric vehicle type", 
    x = "Efficiency (wh/km)", 
    y = "Car Body Type"
  ) + theme_minimal()


print("Box plot created!")