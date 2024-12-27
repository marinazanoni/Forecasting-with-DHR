####### TSCV FOR DHR ########
# IN THIS FILE WE PERFORM TSCV BY SCRATCH FOR DHR
# WITH IMPLEMENT TSCV WITH BOTH FIXED AND NOT FIXED WINDOW IN A REALLY SPECIFIC CASE
# NOT WITH THE GOAL OF IMPLENTING A PROPER FUNCTION.
# RMSE AND MAE METRICS WERE USED FOR TSCV
# SUCH MATRICS WERE USED TO OPERATE A COMPARISON BETWEEN RANDOM FOREST AND DHR


library(lubridate)
library(tsibble)
library(caret)
library(Metrics)
library(randomForest)
library(pdp)
library(doParallel)
library(ModelMetrics)
library(parallel)
library(ggplot2)
library(reshape2)
library(tidyr)
library(future.apply)
library(forecast)
library(tidyverse)
library(dhReg)
library(rlang)

# Set number of cores
numCores <- detectCores() 
cl <- makeCluster(numCores)
registerDoParallel(cl)


rm(list = ls()); load(file= "data.RData")

####SETUP - CUMULATIVE ####

data$Data.Presentazione <- as.Date(data$Data.Presentazione)
#TAKE ONLY MUNICIPIo. drop all NA
municipi <- na.omit(data)
data_filtered_municipi <- municipi %>% filter(Anno != 2020)
data_filtered_municipi <- municipi %>% filter(Anno != 2021)
#Tolgo fino a Marzo 2023
data_filtered_municipi <- data_filtered_municipi %>%
  filter(Data.Presentazione <= as.Date("2023-04-01"))
data_filtered_municipi <- data_filtered_municipi %>%
  filter(Data.Presentazione > as.Date("2021-06-30"))

ts_unique <- data_filtered_municipi %>%
  group_by(Data.Presentazione) %>%
  summarise(Numero_Segnalazioni = n())


ts <- data_filtered_municipi %>%
  group_by(Data.Presentazione, 
           Mese,
           Giorno,
           Anno,
           Area.Tematica,
           Municipio) %>%
  summarise(Numero_Segnalazioni = n())


#XREG
municipio_data <- ts
municipio_data$Data.Presentazione <- as.Date(municipio_data$Data.Presentazione)
municipio_data$Area.Tematica <- ifelse(municipio_data$Area.Tematica == "CASA E URBANISTICA", "ALTRI SERVIZI",
                                       municipio_data$Area.Tematica)  
municipio_data$Area.Tematica <- ifelse(municipio_data$Area.Tematica == "COMMERCIO E IMPRESA", "ALTRI SERVIZI",
                                       municipio_data$Area.Tematica)
municipio_data$Area.Tematica <- ifelse(municipio_data$Area.Tematica == "TURISMO", "ALTRI SERVIZI",
                                       municipio_data$Area.Tematica)
municipio_data$Area.Tematica <- ifelse(municipio_data$Area.Tematica == "DIRITTI E PARI OPPORTUNITÀ", "ALTRI SERVIZI",
                                       municipio_data$Area.Tematica)
municipio_data$Area.Tematica <- ifelse(municipio_data$Area.Tematica == "CULTURA", "ALTRI SERVIZI",
                                       municipio_data$Area.Tematica)
municipio_data <- subset(municipio_data, select = -c(Anno,Mese,Giorno))
#table(municipio_data$Area.Tematica)


# Train-test split
municipio_data_train <- municipio_data %>% filter(Data.Presentazione < as.Date("2023-01-01"))
municipio_data_test <- municipio_data %>% filter(Data.Presentazione >= as.Date("2023-01-01"))
municipio_data_train$Numero_Segnalazioni <- as.numeric(municipio_data_train$Numero_Segnalazioni)

data_aggregated <- municipio_data_train %>%
  group_by(Data.Presentazione, Area.Tematica) %>%
  summarise(Numero_Segnalazioni = sum(Numero_Segnalazioni))

matrice_dati <- data_aggregated %>%
  pivot_wider(names_from = Area.Tematica, values_from = Numero_Segnalazioni, values_fill = 0)

#Inserire 0/1. 1 == Domenica oppure festivo (25 aprile, 25 Dicembre...)
matrice_dati$FESTIVO <- ifelse(weekdays(matrice_dati$Data.Presentazione) == "domenica" |
                                 month(matrice_dati$Data.Presentazione) == 12 & 
                                 day(matrice_dati$Data.Presentazione) == 25 |
                                 month(matrice_dati$Data.Presentazione) == 4 & 
                                 day(matrice_dati$Data.Presentazione) == 25 |
                                 month(matrice_dati$Data.Presentazione) == 8 & 
                                 day(matrice_dati$Data.Presentazione) == 15 |
                                 month(matrice_dati$Data.Presentazione) == 12 & 
                                 (day(matrice_dati$Data.Presentazione) %in% c(24, 26, 31)) |
                                 month(matrice_dati$Data.Presentazione) == 1 & 
                                 (day(matrice_dati$Data.Presentazione) %in% c(1, 6)) |
                                 month(matrice_dati$Data.Presentazione) == 6 & 
                                 day(matrice_dati$Data.Presentazione) == 29 |
                                 month(matrice_dati$Data.Presentazione) == 6 & 
                                 day(matrice_dati$Data.Presentazione) == 2 |
                                 month(matrice_dati$Data.Presentazione) == 5 & 
                                 day(matrice_dati$Data.Presentazione) == 1 |
                                 month(matrice_dati$Data.Presentazione) == 11 & 
                                 day(matrice_dati$Data.Presentazione) == 1, 1, 0)


matrice_dati$POSTFESTIVO <- ifelse(month(matrice_dati$Data.Presentazione) == 4 & 
                                     day(matrice_dati$Data.Presentazione) == 26 |
                                     month(matrice_dati$Data.Presentazione) == 12 & 
                                     day(matrice_dati$Data.Presentazione) == 27 |
                                     month(matrice_dati$Data.Presentazione) == 8 & 
                                     day(matrice_dati$Data.Presentazione) == 16 |
                                     month(matrice_dati$Data.Presentazione) == 1 & 
                                     day(matrice_dati$Data.Presentazione) == 7 |
                                     month(matrice_dati$Data.Presentazione) == 6 & 
                                     day(matrice_dati$Data.Presentazione) == 30 |  
                                     month(matrice_dati$Data.Presentazione) == 6 &
                                     day(matrice_dati$Data.Presentazione) == 3 |
                                     month(matrice_dati$Data.Presentazione) == 5 & 
                                     day(matrice_dati$Data.Presentazione) == 2 |
                                     (weekdays(matrice_dati$Data.Presentazione) == "lunedì" &
                                        matrice_dati$FESTIVO == 0 )   |
                                     month(matrice_dati$Data.Presentazione) == 11 & 
                                     day(matrice_dati$Data.Presentazione) == 2, 1,0 )

matrice_dati$MESE <- as.integer(format(matrice_dati$Data.Presentazione, "%m"))
matrice_dati <- matrice_dati %>%
  mutate(across(everything(), as.numeric))
ordered_columns <- order(names(matrice_dati))

matrice_dati <- matrice_dati[, ordered_columns]
matrice_dati <- subset(matrice_dati, select = -Data.Presentazione)

# TARGET VARIABLE
municipio_dhr <- ts_unique
municipio_dhr_train <- municipio_dhr %>% filter(Data.Presentazione < as.Date("2023-01-01"))
municipio_dhr_test <- municipio_dhr %>% filter(Data.Presentazione >= as.Date("2023-01-01"))
municipio_dhr_train$Data.Presentazione <- as.Date(municipio_dhr_train$Data.Presentazione)
new_data <- data.frame(  Numero_Segnalazioni = municipio_dhr_train$Numero_Segnalazioni)
rownames(new_data) <- municipio_dhr_train$Data.Presentazione

########################### TSCV - FIXED WINDOW #########################à

step_size <- 90
horizon <- 90
results <- list()


# TSCV - Loop 
n <- nrow(new_data)
for (start in seq(1, n - horizon + 1, by = step_size)) {
  end <- start + init_window - 1
  print(c("s",start,"e",end))
  
  # original matrix
  train <- new_data[start:end, ]
  test <- new_data[(end + 1):(end + horizon), ]
  
  # auxiliary matrix
  x_train <- matrice_dati_matrix[start:end, ]
  
  
  # Addestra il modello (ad esempio, un modello ARIMA)
  fit <- dhr(train,
             XREG = x_train[,c(2,3,4,5)],        # Independent Variables 
             #XREG=NULL,
             Range = list(1:5, 1),  # Range  Fourier
             Frequency = c(365),    # Seasonality
             Criteria = "aicc",     # Selection criteria
             maxp = 5,              # max AR coefficients in auto.arima
             maxq = 5,              # max MA coefficients in auto.arima
             maxd = 5)              # max integration in modello auto.arima
  
  # Predictions
  forecast <- fc(Frequency = c(365), 
                 XREG_test = x_train[,c(2,3,4,5)], 
                 h = length(train), 
                 Fit = fit, 
                 Data = new_data$Numero_Segnalazioni)
  
  pred <- forecast$mean[1:horizon]
  
  # Compute the error (for example, RMSE)
  mse <- mean((pred - new_data$Numero_Segnalazioni[(end+1):(end+horizon)])^2, na.rm = TRUE)
  mse_aut <- mse(actual = new_data$Numero_Segnalazioni[(end+1):(end+horizon)], predicted = pred)
  
  # Compute total RMSE
  total_rmse <- mean(rmse_values)
    
  # Save the results
  results[[length(results) + 1]] <- list(train_data = train, 
                                   forecast = pred, 
                         residuals = pred - new_data$Numero_Segnalazioni[(end+1):(end+horizon)],
                                         mse = mse, mse_aut=mse_aut)
}


#  AVERAGE cross-validation ERROR 
results[[1]]$rmse
rmse     #[1] 267.8127 - 
rmse_values <- sapply(results, function(x) round(x$rmse))
rmse_values
# TOTAL RMSE
total_rmse <- mean(rmse_values)

result_reduced<-results
save(result_reduced,     file = "res_reduced.RData")


########################### TSCV - ROLLING WINDOW #########################à

matrice_dati_matrix <- as.matrix(matrice_dati)
results<-list()

# Loop
n <- nrow(new_data)
for (i in 1:ceiling((n - horizon) / step_size)) {
  start <- 1
  end <- horizon  + (i -1) * step_size
  
  if (end + horizon > n) end<- n - horizon 
  
  print(c("s", start, "e", end))
  
  # original matrix
  train <- new_data[start:end, ]
  test <- new_data[(end + 1):(end + horizon), ]
  
  # auxiliary matrix
  x_train <- matrice_dati_matrix[start:end, ]
  
  
  # Fit your modello (ad esempio, un modello ARIMA)
  fit <- dhr(train,
             XREG = x_train[,c(1,2,3,4,6,7,9,10)],      
             Range = list(1:5, 1),  
             Frequency = c(365),    
             Criteria = "aicc",     
             maxp = 5,              
             maxq = 5,              
             maxd = 5)              
  
 
  forecast <- fc(Frequency = c(365), 
                 XREG_test = x_train[,c(1,2,3,4,6,7,9,10)], 
                 h = length(train), 
                 Fit = fit, 
                 Data = new_data$Numero_Segnalazioni)
  
  pred <- forecast$mean[1:horizon]
  
  rmse <- sqrt(mean((pred - test)^2, na.rm = TRUE))
  
  # Results
  results[[length(results) + 1]] <- list(train_data = train, 
                                         forecast = pred, 
                                         residuals = pred - new_data$Numero_Segnalazioni[(end+1):(end+horizon)],
                                         rmse = rmse)
}


################################ FINE TUNING #############################


matrice_dati_matrix <- as.matrix(matrice_dati)
# Parameters
step_size <- 90
horizon <- 90
rmse_values<-c()
best_model_results <- list()
best_rmse <- Inf

# Parameters to optimize
params <- expand.grid(maxp = c(3, 5, 7, 10), 
                      maxq = c(3, 5, 7, 10), 
                      maxd = c(3, 5, 7, 10))

# Data frame to save the final RMSE values and corresponding parameters
final_results <- data.frame(maxp = integer(), 
                            maxq = integer(), 
                            maxd = integer(), 
                            rmse = double(), 
                            fold_rmse = I(list()), 
                            stringsAsFactors = FALSE)

# Loop through parameter sets
for (i in 1:nrow(params)) {
  param_set <- params[i, ]
  cat("Iteration:", i, 
      "Current Parameters: maxp =", param_set$maxp, 
      "maxd =", param_set$maxd, 
      "maxq =", param_set$maxq, "\n")
  
  fold_rmse_values <- list()
  
  n <- nrow(new_data)
  for (j in 1:ceiling((n - horizon) / step_size)) {
    start <- 1
    end <- horizon + (j - 1) * step_size
    
    # Ensure that 'end' does not exceed the number of rows
    if (end + horizon > n) break
    
    # Training dataset
    train <- new_data[1:end, ]
    test <- new_data[(end + 1):(end + horizon), ]
    
    # Auxiliary training matrix
    x_train <- matrice_dati_matrix[1:end, ]
    
    # Train the model
    fit <- dhr(train,
               XREG = x_train[, c(1, 2, 3, 4, 6, 7, 9, 10)],
               Range = list(1:5, 1),
               Frequency = c(365),
               Criteria = "aicc",
               maxp = param_set$maxp,
               maxq = param_set$maxq,
               maxd = param_set$maxd)
    
    # Forecast on test data
    forecast <- fc(Frequency = c(365), 
                   XREG_test = x_train[, c(1, 2, 3, 4, 6, 7, 9, 10)], 
                   h = length(train), 
                   Fit = fit, 
                   Data = train)
    
    pred <- forecast$mean[1:horizon]
    
    # Calculate forecast error
    mse_value <- mse(test, pred)
    rmse_value <- sqrt(mse_value)
    rmse_values <- c(rmse_values, rmse_value)
    fold_rmse_values[[j]] <- rmse_value
  }
  
  final_rmse <- mean(rmse_values, na.rm = TRUE)
  
  # Save results for the current parameter set
  final_results <- rbind(final_results,
                         data.frame(maxp = param_set$maxp,
                                    maxq = param_set$maxq,
                                    maxd = param_set$maxd,
                                    rmse = final_rmse,
                                    fold_rmse = I(list(fold_rmse_values))))
  
  if (final_rmse < best_rmse) {
    best_rmse <- final_rmse
    best_model_results <- list(
      train_data = train,
      test_data = test,
      forecast = pred,
      residuals = pred - test
    )
  }
}


#############################  RESULTS AND PLOTS #############################

# Ensure `mean_rmse` and `fold_rmse` are treated as numeric
final_results$mean_rmse <- sapply(final_results$fold_rmse, function(x) mean(unlist(x), na.rm = TRUE))
final_results$mean_rmse <- as.numeric(final_results$mean_rmse)

# Unnest the `fold_rmse` column for plotting
final_results_unnested <- final_results %>%
  unnest(fold_rmse) %>%
  mutate(parameter_set = interaction(maxp, maxq, maxd))

# Ensure `fold_rmse` is numeric in the unnested data frame
final_results_unnested$fold_rmse <- as.numeric(final_results_unnested$fold_rmse)

# Select top 5 parameter sets based on `mean_rmse`
top5_params <- final_results %>%
  arrange(rmse) %>%
  head(5)

top5_unnested <- final_results_unnested %>%
  filter(parameter_set %in% interaction(top5_params$maxp, top5_params$maxq, top5_params$maxd))

# Ensure `mean_rmse` and `fold_rmse` in `top5_unnested` are numeric
top5_unnested$fold_rmse <- as.numeric(top5_unnested$fold_rmse)
top5_params$mean_rmse <- as.numeric(top5_params$mean_rmse)

# Create the combined line plot for the top 5 parameter sets
ggplot(top5_unnested, aes(x = parameter_set, y = fold_rmse)) +
  geom_boxplot(alpha = 0.7, fill = "lightblue", color = "black") +
  geom_line(data = top5_params, aes(x = interaction(maxp, maxq, maxd), 
                                    y = mean_rmse, group = 1), color = "blue", size = 1.5) +
  geom_point(data = top5_params, aes(x = interaction(maxp, maxq, maxd), y = mean_rmse), color = "red", size = 3) +
  labs(title = "Top 5 RMSE values across folds for different parameter sets",
       x = "Parameter Sets (maxp, maxq, maxd)",
       y = "RMSE") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))


# Determine the best parameter set
best_params <- final_results[which.min(final_results$rmse), ]
print(best_params)

save(final_results, file = "final_results-1-5.RData")
save(final_results, file = "final_results-1-3-5-10.RData")

# TEMATIC AREAS ----------------------------------------
# CONDISDER CASE FOR 1 SPECIFIC TEMATIC AREA 

rm(list=ls()); load(file= "data.RData")

# USE SAME MANIPOLATIONS OF THE DATA THAT THOSE YOU CAN FIND ON DHR_MODELS

################################ SICUREZZA URBANA Municipio I #############################

# Implementation TSCV with rolling windows
# Parameters
step_size <- 90
horizon <- 90
matrice_dati_matrix <- as.matrix(matrice_dati)

# Loop 
n <- nrow(new_data)
for (i in 1:ceiling((n - horizon) / step_size)) {
  start <- 1
  end <- horizon  + (i -1) * step_size
  
  # Assicurati che l'end non superi il numero di righe
  if (end + horizon > n) end<- n - horizon 
  
  print(c("s", start, "e", end))
  
  # original matrix
  train <- municipio_multivariata[start:end, ]
  test <- municipio_multivariata[(end + 1):(end + horizon), ]
  
  # auxiliary matrix
  x_train <- matrice_dati_matrix[start:end, ]
  
  fit<-dhr(train$Numero_Segnalazioni,
            XREG = x_train[,c(3,5)],        
            Range = list(1:5, 1),   
            Frequency = c(365),         
            Criteria = "aicc",          
            maxp = 5,                    
            maxq = 5,                   
            maxd = 5) 
  
  # Prevedi sui dati di test
  forecast <- fc(Frequency = c(365), 
                 XREG_test = x_train[,c(3,5)], 
                 h = length(train$Numero_Segnalazioni), 
                 Fit = fit, 
                 Data = train$Numero_Segnalazioni)
  
  pred <- forecast$mean[1:horizon]
  
  rmse <- sqrt(mean((pred - test$Numero_Segnalazioni)^2, na.rm = TRUE))
  
  # Salva i risultati
  results[[length(results) + 1]] <- list(train_data = train$Numero_Segnalazioni, 
                                         forecast = pred, 
                                         residuals = pred - test$Numero_Segnalazioni,
                                         rmse = rmse)
}

############################# RESULTS  #############################

rmse_values <- sapply(results, function(x) (x$rmse))
# 8.049591   (a bit higher that performing the other cross-validation)
total_rmse <- mean(rmse_values)
print(total_rmse)
#18.29807


# Create a combined data frame
combined_df <- do.call(rbind, lapply(1:length(results), function(i) {
  data.frame(
    train_data = results[[i]]$train_data[1:horizon],
    forecast = results[[i]]$forecast[1:horizon],
    residuals = results[[i]]$residuals[1:horizon],
    fold = paste0("Fold ", i)
  )
}))

# Melt the data frame for plotting
melted_df <- melt(combined_df, id.vars = "fold")

# Plotting
ggplot(melted_df, aes(x = value, color = fold)) +
  geom_density() +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribution Evolution Over Time",
       x = "Value",
       y = "Density") +
  theme_minimal()

# ACF/PACF of Residuals for each fold
par(mfrow = c(2, 2))
for (i in 1:4) {
  acf(results[[i]]$residuals[1:90], main = paste("ACF of Residuals - Fold", i))
}

rmse_values

############################## GESTIONE RIFIUTI Municipio VII  #############################

set.seed(123)
step_size <- 180
horizon <- 90
matrice_dati_matrix <- as.matrix(matrice_dati)
results<- list()

# Loop attraverso i dati
n <- nrow(new_data)
for (i in 1:ceiling((n - horizon) / step_size)) {
  start <- 1
  end <- horizon  + (i -1) * step_size
  
  # Assicurati che l'end non superi il numero di righe
  if (end + horizon > n) end<- n - horizon 
  
  print(c("s", start, "e", end))
  
  
  # original matrix
  train <- municipio_multivariata[start:end, ]
  test <- municipio_multivariata[(end + 1):(end + horizon), ]
  
  # auxiliary matrix
  x_train <- matrice_dati_matrix[start:end, ]
  
  
  fit<-dhr(train$Numero_Segnalazioni,
           XREG = x_train[,c(3,5)],        
           Range = list(1:5, 1),   
           Frequency = c(365),         
           Criteria = "aicc",          
           maxp = 5,                    
           maxq = 5,                   
           maxd = 5) 
  
  # Prevedi sui dati di test
  forecast <- fc(Frequency = c(365), 
                 XREG_test = x_train[,c(3,5)], 
                 h = length(train$Numero_Segnalazioni),  
                 Fit = fit, 
                 Data = train$Numero_Segnalazioni)
  
  pred <- forecast$mean[1:horizon]
  
  rmse <- sqrt(mean((pred - test$Numero_Segnalazioni)^2, na.rm = TRUE))
  mae<- mean(abs(pred - test$Numero_Segnalazioni),na.rm=TRUE)
  
  # Salva i risultati
  results[[length(results) + 1]] <- list(train_data = train$Numero_Segnalazioni, 
                                         forecast = pred, 
                                         residuals = pred - test$Numero_Segnalazioni,
                                         rmse = rmse,
                                         mae=mae)
}


rmse_values <- sapply(results, function(x) (x$rmse))
rmse_values

#total_rmse <- mean(rmse_values[-c(2)])
total_rmse <- mean(rmse_values)
#total_rmse <- mean(rmse_values[-c(2,4,6)])   # 6.78 30-90
print(total_rmse)
#5.36196
# MAYBE KEEP 180-90 -->error=6
#13.92296 with 120-90, seems too sensitive to cross validation...
# by removing one extremly problematic element


# Create a combined data frame
combined_df <- do.call(rbind, lapply(3, function(i) {
  data.frame(
    train_data = results[[i]]$train_data[1:horizon],
    forecast = results[[i]]$forecast[1:horizon],
    residuals = results[[i]]$residuals[1:horizon],
    fold = paste0("Fold ", i)
  )
}))



# Melt the data frame for plotting
melted_df <- melt(combined_df, id.vars = "fold")

# Plotting
ggplot(melted_df, aes(x = value)) +
  geom_density() +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribution Evolution Over Time",
       x = "Value",
       y = "Density") +
  theme_minimal()

# ACF/PACF of Residuals for each fold
par(mfrow = c(2, 1))
for (i in 1:2) {
  acf(results[[i]]$residuals[1:90], main = paste("ACF of Residuals - Fold", i))
}

save(results, file = "results-train.RData")


