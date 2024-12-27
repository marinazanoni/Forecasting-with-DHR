#### RANDOM FOREST #####
#install.packages("randomForest")
library(dplyr)
library(lubridate)
library(tsibble)
library(caret)
library(randomForest)
library(tidyr)
library(pdp)
#install.packages("doParallel")
library(doParallel)
#R.version.string
library(parallel)
num_cores <- detectCores()  # Detect the number of available cores
print(num_cores)

set.seed(123)

#head(data)
#View(data)

#data <- subset(data, Anno != 2020)
#data <- subset(data, Anno != 2023)

#rm(list=ls())

#TAKE ONLY MUNICIPIo. drop all NA
municipi <- na.omit(data)
data_filtered_municipi <- municipi %>% filter(Anno != 2020)
#Tolgo fino a Marzo 2023
data_filtered_municipi <- data_filtered_municipi %>%
  filter(Data.Presentazione <= as.Date("2023-04-01"))

data_filtered_municipi <- data_filtered_municipi %>%
  filter(Data.Presentazione > as.Date("2021-06-30"))


# Multivariate time series -----------------------
suppressMessages({
  ts <- data_filtered_municipi %>%
    group_by(`Data.Presentazione`,
             `Area.Tematica`,
             `Municipio`) %>%
    summarise(Numero_Segnalazioni = n())
})
ts

# reducing to the five thematic areas -------------------------


municipio_data <- subset(ts, Municipio== 7)

municipio_data$Data.Presentazione <- as.Date(municipio_data$Data.Presentazione)

municipio_data$Area.Tematica <- ifelse(municipio_data$Area.Tematica == "CASA E URBANISTICA", "ALTRI SERVIZI", municipio_data$Area.Tematica)  
municipio_data$Area.Tematica <- ifelse(municipio_data$Area.Tematica == "COMMERCIO E IMPRESA", "ALTRI SERVIZI", municipio_data$Area.Tematica)
municipio_data$Area.Tematica <- ifelse(municipio_data$Area.Tematica == "TURISMO", "ALTRI SERVIZI", municipio_data$Area.Tematica)
municipio_data$Area.Tematica <- ifelse(municipio_data$Area.Tematica == "DIRITTI E PARI OPPORTUNITÀ", "ALTRI SERVIZI", municipio_data$Area.Tematica)
municipio_data$Area.Tematica <- ifelse(municipio_data$Area.Tematica == "CULTURA", "ALTRI SERVIZI", municipio_data$Area.Tematica)

# considering 3-month prediction to see performances --------------------

#Train-test split
municipio_data_train <- municipio_data %>% filter(Data.Presentazione < as.Date("2023-01-01"))
municipio_data_test <- municipio_data %>% filter(Data.Presentazione >= as.Date("2023-01-01"))

municipio_data_train$Numero_Segnalazioni <- as.numeric(municipio_data_train$Numero_Segnalazioni)


data_aggregated <- municipio_data_train %>%
  group_by(Data.Presentazione, Area.Tematica) %>%
  summarise(Numero_Segnalazioni = sum(Numero_Segnalazioni))

matrice_dati <- data_aggregated %>%
  pivot_wider(names_from = Area.Tematica, values_from = Numero_Segnalazioni, values_fill = 0)


matrice_dati <- subset(matrice_dati, select = -Data.Presentazione)


matrice_dati_matrix <- as.matrix(matrice_dati)


str(matrice_dati_matrix)
#View(matrice_dati)

# TS --------------------------------------
##Multivariate
summary(municipio_data_train)   # no NAs

# set parameter for tuning without tscv
trainControl_param <- trainControl(
  method = "oob", 
  number = 20, 
  search = "random")

# Define the grid of hyperparameters to search
tuneGrid <- expand.grid(
  .mtry = c(2, 3, 4, 5)  # Example values
)


head(municipio_data_train)
municipio_data_train <- subset(municipio_data_train, select = -Municipio)

# Train the model using the grid of parameters defined before
model_trial_multi <- train(
  Numero_Segnalazioni ~ ., 
  data = municipio_data_train, 
  method = "rf", 
  trControl = trainControl_param, 
  tuneGrid = tuneGrid, 
  metric = 'RMSE', 
  importance = TRUE,
  ntree = 200  # Set ntree directly
)

# Predict on the training set
predicted_train_ts <- predict(model_trial_multi, municipio_data_train)

# Ensure factor levels in the test set match those in the training set
#test_ts$Area.Tematica <- factor(test_ts$Area.Tematica, levels = levels(train_ts$Area.Tematica))

# Predict on the test set
predicted_test_ts <- predict(model_trial_multi, municipio_data_test)
municipio_data_test$predicted_test <- predicted_test_ts
sqrt(mean((municipio_data_test$Numero_Segnalazioni - municipio_data_test$predicted_test)^2)) ### 5.667607
municipio_data_test[20:40,]


curiosity_dataframe<-data.frame(municipio_data_test)
View(curiosity_dataframe)


# CROSS VALIDATION ----------------------------------
### Trial for Cross Validation
municipio_data_train$month <- as.factor(month(municipio_data_train$Data.Presentazione))
municipio_data_test$month <- as.factor(month(municipio_data_test$Data.Presentazione))

#### creating sampling seeds ####
seeds <- vector(mode = "list", length = nrow(municipio_data_train))
for(i in 1:(nrow(municipio_data_train)-1)) seeds[[i]] <- sample.int(1000, 5)
registerDoParallel(cores=2)

# FIRST ATTEMPT -------------------------------
# cv step rolling average 
myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 30,
                              horizon = 7,
                              fixedWindow = FALSE,
                              allowParallel = TRUE,
                              seeds = seeds)

tuneLength.num <- 5
head(municipio_data_train)
trainX <- municipio_data_train[,c("Area.Tematica", "Data.Presentazione", "month")]
trainY <- municipio_data_train[,"Numero_Segnalazioni",drop=TRUE]

rf.mod <- train(x=trainX,
                y=trainY,
                method = "rf",
                trControl = myTimeControl,
                tuneLength=tuneLength.num,
                metric='RMSE')
rf.mod

tuneGrid <- data.frame(mtry = 2)
selected_model <- train(
  Numero_Segnalazioni ~ ., 
  data = municipio_data_train, 
  method = "rf", 
  trControl = myTimeControl, 
  tuneGrid = tuneGrid, 
  metric = 'RMSE', 
  importance = TRUE,
  ntree = 200  # Set ntree directly
)

#tuneGrid <- data.frame(mtry = 2)
rf2 <- train(
  Numero_Segnalazioni ~ ., 
  data = municipio_data_train, 
  method = "rf", 
  trControl = myTimeControl, 
  tuneGrid = tuneGrid, 
  metric = 'RMSE', 
  importance = TRUE,
  ntree = 50  # Set ntree directly
)

# TRYING TUNEGRID ------------------------
# Define the grid of parameters to tune
tuneGrid <- expand.grid(
  mtry = 1:2#,                # Number of variables randomly sampled as candidates at each split
  #ntree = c(100, 200, 300),  # Number of trees in the forest
  #nodesize = c(1, 5, 10)     # Minimum size of terminal nodes
)


# Train the Random Forest model with tuning
rf.mod <- train(
  Numero_Segnalazioni ~ Area.Tematica + Data.Presentazione + month,
  data = municipio_data_train,
  method = "rf",
  trControl = myTimeControl,
  tuneGrid = tuneGrid,
  metric = 'RMSE',
  importance = TRUE
)

predicted_test_ts_cv <- predict(selected_model, test_ts)


# CODE FOR FOR LOOPS ---------------------------
# to be changed for out purposes

# Custom grid search parameters
ntreeGrid <- c(200, 250)
nodesizeGrid <- c(1, 5)
mtryGrid <- data.frame(mtry = 1:3)

myTimeControl <- trainControl(
  method = "timeslice",
  initialWindow = 36,
  horizon = 12,
  fixedWindow = FALSE,
  allowParallel = TRUE
)

# Calculate the number of resamples
numResamples <- (nrow(municipio_data_train) - 30) / 7

# Construct the seeds list
seeds <- vector(mode = "list", length = numResamples + 1)
for(i in 1:numResamples) seeds[[i]] <- sample.int(1000, length(mtryGrid$mtry) * length(ntreeGrid) * length(nodesizeGrid))
seeds[[numResamples + 1]] <- sample.int(1000, 1)

# Initialize variables to store the best model and its performance
bestModel <- NULL
bestRMSE <- Inf
bestParams <- list()

# Train the Random Forest model with custom grid search
for (ntree in ntreeGrid) {
  for (nodesize in nodesizeGrid) {
    set.seed(123)
    rf.mod <- train(
      Numero_Segnalazioni ~ Area.Tematica + Data.Presentazione + month,
      data = municipio_data_train,
      method = "rf",
      trControl = myTimeControl,
      tuneGrid = mtryGrid,
      metric = 'RMSE',
      importance = TRUE,
      ntree = ntree,
      nodesize = nodesize
    )
    
    # Get the best RMSE for the current model
    currentBestRMSE <- min(rf.mod$results$RMSE)
    
    # Update the best model if the current one is better
    if (currentBestRMSE < bestRMSE) {
      bestRMSE <- currentBestRMSE
      bestModel <- rf.mod
      bestParams <- list(ntree = ntree, nodesize = nodesize)
    }
  }
}

# Varible importance -------------------------
# Print the best parameters and the best model's RMSE
print(bestParams)
print(bestModel)

# Extract variable importance
importance <- varImp(bestModel, scale = FALSE)
print(importance)

# Calculate correlation between predicted and actual values
predictions <- predict(bestModel, newdata = municipio_data_train)
actuals <- municipio_data_train$Numero_Segnalazioni
correlation <- cor(predictions, actuals)
print(correlation)

# Plot variable importance
plot(importance)


# Model Selection -------------------------

#Finally, we select the best model based on cross-validation 𝑅𝑀𝑆𝐸. For time series problems, you should avoid using 𝑅2 as a selection criteria due to the spurious regression problem: regressing two time series that have strong deterministic or stochastic trends lead to severely inflated 𝑅2 values.

# change with OUR list of models
resamps <- resamples(list(rf1 = selected_model,
                          randomforest2 = rf2))



ss <- summary(resamps)

knitr::kable(ss[[3]]$RMSE)

trellis.par.set(caretTheme())
dotplot(resamps, metric = "RMSE")


lapply(c("Area.Tematica","Data.Presentazione","month"), function(x){
  partial(selected_model, pred.var=x, plot=TRUE, train=trainX)
}
)


# SIMPLE TRIES ON SUBSET OF THE DATASET OT GET THE LOGIC----------

# Define a custom summary function that calculates RMSE
customRMSE <- function(data, lev = NULL, model = NULL) {
  rmse <- RMSE(data$pred, data$obs)
  out <- c(RMSE = rmse)
  return(out)
}


# Subset the data for quicker testing
set.seed(123)  # For reproducibility
subset_size <- 100  # Define the size of the subset
subset_indices <- sample(nrow(municipio_data_train), subset_size)
subset_indices
data_subset <- municipio_data_train[subset_indices, ]

# Add the 'month' variable
data_subset$month <- as.numeric(format(data_subset$Data.Presentazione, "%m"))

# Define train control with custom summary function
myTimeControl <- trainControl(
  method = "timeslice",
  initialWindow = 36,
  horizon = 12,
  fixedWindow = FALSE,
  allowParallel = TRUE,
  summaryFunction = customRMSE
)

# Define the parameter grid for tuning
tuneGrid <- expand.grid(
  mtry = 1:3,
  splitrule = "variance",  # For regression tasks
  min.node.size = c(1, 3)
)

# Initialize variables to store the best model and its performance
bestModel <- NULL
bestRMSE <- Inf
bestParams <- list()

# Train the Random Forest model with custom grid search
for (ntree in c(100, 150)) {  # Reduced for quicker testing
  set.seed(123)
  rf.mod <- train(
    Numero_Segnalazioni ~ Area.Tematica + Data.Presentazione + month,
    data = data_subset,
    method = "ranger",
    trControl = myTimeControl,
    tuneGrid = tuneGrid,
    metric = 'RMSE',
    importance = 'impurity',
    num.trees = ntree
  )
  
  # Get the best RMSE for the current model
  currentBestRMSE <- min(rf.mod$results$RMSE)
  
  # Update the best model if the current one is better
  if (currentBestRMSE < bestRMSE) {
    bestRMSE <- currentBestRMSE
    bestModel <- rf.mod
    bestParams <- list(ntree = ntree, mtry = rf.mod$bestTune$mtry, min.node.size = rf.mod$bestTune$min.node.size, splitrule = rf.mod$bestTune$splitrule)
  }
}

# Print the best parameters and the best model's RMSE
print(bestParams)
print(bestModel)


# SEE WHAT IS BEST MODEL
str(bestModel)
# I can access the RMSE of all mode

# EVEN IF DOING SO I CAN ACCESS ALL THE INFORMATION OF THE SINGLE STEPS
rmse_values<-bestModel$results$RMSE
rmse_values
  
# Initialize lists to store training and testing indices for each timeslice
training_subfolds <- list()
testing_subfolds <- list()

# Access the training and testing indices for each timeslice from the model object
training_indices <- bestModel$control$index
testing_indices <- bestModel$control$indexOut

# Iterate over the indices to extract the corresponding data
for (i in seq_along(training_indices)) {
  train_index <- training_indices[[i]]
  test_index <- testing_indices[[i]]
  
  # Extract the training and testing data for the current timeslice
  train_data <- municipio_data_train[train_index, ]
  test_data <- municipio_data_train[test_index, ]
  
  # Store the data in the lists
  training_subfolds[[i]] <- train_data
  testing_subfolds[[i]] <- test_data
}

# Print the first training and testing subfold
print(training_subfolds[[1]])
print(testing_subfolds[[1]])

# Save the first training and testing subfold to CSV files
write.csv(training_subfolds[[1]], file = "training_subfold_1.csv", row.names = FALSE)
write.csv(testing_subfolds[[1]], file = "testing_subfold_1.csv", row.names = FALSE)

# Extract variable importance
importance <- varImp(bestModel, scale = FALSE)
print(importance)

# Calculate correlation between predicted and actual values
predictions <- predict(bestModel, newdata = data_subset)
actuals <- data_subset$Numero_Segnalazioni
correlation <- cor(predictions, actuals)
print(correlation)

# Plot variable importance
plot(importance)

# Generate Partial Dependence Plots
pdp_area_tematica <- partial(bestModel, pred.var = "Area.Tematica", grid.resolution = 10)
pdp_data_presentazione <- partial(bestModel, pred.var = "Data.Presentazione", grid.resolution = 10)
pdp_month <- partial(bestModel, pred.var = "month", grid.resolution = 10)

# Plot Partial Dependence Plots
plotPartial(pdp_area_tematica)
plotPartial(pdp_data_presentazione)
plotPartial(pdp_month)

# Save the important objects to an RData file
save(bestModel, 
     bestParams, 
     importance, 
     pdp_area_tematica, 
     pdp_data_presentazione, 
     pdp_month, 
     file = "important_objects.RData")

# CHECKING THE RESIDUALS -----------------------------
residuals <- municipio_data_train[subset_indices,]$Numero_Segnalazioni - predictions

# Combine the results in a data frame
results_df <- data.frame(
  Actual = municipio_data_train[subset_indices,]$Numero_Segnalazioni,
  Predicted = predictions,
  Residuals = residuals
)

results_df
plot(results_df$Residuals)
