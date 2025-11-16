library(dplyr)
library(ggplot2)
library(tidymodels)
library(ranger)
library(randomForest)
library(gbm)

data<-read.csv("/Users/simonasarliukas/Desktop/Data mining/electric_vehicles_spec_2025.csv.csv")
head(data)


data_numeric<-data[, sapply(data, is.numeric)]

data_numeric %>%
  pivot_longer(-efficiency_wh_per_km, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, y = efficiency_wh_per_km)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~variable, scales = "free_x") +
  theme_minimal()

data["Charging_time"]<-data$battery_capacity_kWh/(0.95*data$fast_charging_power_kw_dc)

size<-(data$length_mm)/100*(data$width_mm)/100



data["Car_size"]=size*(data$height_mm)/100

data_filtered<-data %>% 
  select(top_speed_kmh,Car_size,Charging_time,efficiency_wh_per_km,torque_nm,acceleration_0_100_s) %>% 
  filter(efficiency_wh_per_km<300)

data_filtered

hist(data_filtered$efficiency_wh_per_km, main="Efficiency Histogram",xlab="Efficiency")

data_filtered["efficiency_wh_per_km"]<-log(data_filtered["efficiency_wh_per_km"])

data_filtered<-drop_na(data_filtered)
data_filtered
hist(data_filtered$efficiency_wh_per_km,main="log Efficiency Histogram",xlab="Efficiency")

qqnorm(data_filtered$efficiency, pch = 1, frame = FALSE)
qqline(data_filtered$efficiency)

#data_filtered["efficiency"]<-100*data$Energy_Consumption_kWh/data$Distance_Travelled_km
#data_filtered<-data_filtered %>% 
#  filter(efficiency<100)
#data_filtered
set.seed(431)
sample<-sample(c(TRUE,F),nrow(data_filtered),replace=T,prob=c(0.7,0.3))
train<-data_filtered[sample,]
test<-data_filtered[!sample,]
data_filtered
var(data_filtered$efficiency_wh_per_km)




tree_spec<-decision_tree() %>% 
  set_engine("rpart") %>%
  set_mode("regression")

train

tree_fit<-tree_spec %>% 
  fit(efficiency_wh_per_km~Car_size+torque_nm+Charging_time+top_speed_kmh+acceleration_0_100_s,data=train)

predictions <- tree_fit %>%
  predict(test) %>%
  pull(.pred)
predictions


tree_fit

metrics <- metric_set(rmse, rsq)
model_performance <- test %>%
  mutate(predictions = predictions) %>%
  metrics(truth = efficiency_wh_per_km, estimate = predictions)
model_performance
tree_fit

rf_with_seed <- 
  rand_forest(trees = 2000, mtry = tune(), mode = "regression") |>
  set_engine("ranger", seed = 63233)

rf_with_seed |> 
  set_args(mtry = 2) |> 
  set_engine("randomForest") |>
  fit(efficiency_wh_per_km~torque_nm+Charging_time+top_speed_kmh+acceleration_0_100_s,data=train)



set.seed(321)
gbm_fit <- gbm(
  formula = efficiency_wh_per_km~torque_nm+Charging_time+top_speed_kmh+acceleration_0_100_s,
  data = train,
  distribution = "gaussian",  # regression
  n.trees = 30000,             # number of boosting iterations
  interaction.depth = 2,       # depth of each tree
  shrinkage = 0.01,            # learning rate
  n.minobsinnode = 10,         # min obs per node
  cv.folds = 5,                # cross-validation folds
  verbose = TRUE
)

best_iter <- gbm.perf(gbm_fit, method = "cv")
best_iter
# Predict
preds <- predict(gbm_fit, newdata = test, n.trees = best_iter)
exp(test)

sqrt(sum((exp(test$efficiency_wh_per_km)-exp(preds))^2)/(length(preds)-1))

