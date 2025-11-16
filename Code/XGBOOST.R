library(xgboost)
library(dplyr)


data <-read.csv("C:/Users/simon/Desktop/Data_mining_Simonas/data-mining-project/dataset/data_cleaned.csv")

numeric_df <- data[, sapply(data, is.numeric)] #Getting numeric features

numeric_df <- numeric_df%>% select(!starts_with("log")) ##Dropping variables that start with log

numeric_df$log_efficiency<-log(numeric_df$efficiency_wh_per_km)

hist(numeric_df$log_efficiency) #Log efficiency is normally distributed.


set.seed(123)
training_set <- sample(1:nrow(numeric_df), size = 0.7 * nrow(numeric_df))

#Converting into a matrix for XGBoost to work

y <- numeric_df$log_efficiency
X <- as.matrix(numeric_df %>% select(-log_efficiency))


dtrain <- xgb.DMatrix(X[training_set, ], label = y[training_set])
dtest  <- xgb.DMatrix(X[-training_set, ], label = y[-training_set])

##Defining parameters

params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1, ##Learning rate
  gamma=0, ##Requiring minimum gain to split the tree
  max_depth = 2,
  lambda=1
   
)


model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 130,
  watchlist = list(train = dtrain, test = dtest),
  print_every_n = 20
)





