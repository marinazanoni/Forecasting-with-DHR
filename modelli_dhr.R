library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
#source("dhReg.R")
library(future.apply)
library(forecast)
library(tidyverse)
library(dhReg)
library(rlang)

load(file= "data.RData")

#fix date
data$Data.Presentazione <- as.Date(data$Data.Presentazione)

#TAKE ONLY MUNICIPIo. drop all NA
municipi <- na.omit(data)
data_filtered_municipi <- municipi %>% filter(Anno != 2020)
#Tolgo fino a Marzo 2023
data_filtered_municipi <- data_filtered_municipi %>%
  filter(Data.Presentazione <= as.Date("2023-04-01"))

data_filtered_municipi <- data_filtered_municipi %>%
  filter(Data.Presentazione > as.Date("2021-06-30"))


#multivariata con municipio
ts <- data_filtered_municipi %>%
  group_by(Data.Presentazione, 
           Mese,
           Giorno,
           Anno,
           Area.Tematica,
           Municipio) %>%
  summarise(Numero_Segnalazioni = n())
###########################################
#XREG

municipio_data <- subset(ts, Municipio== 5)

municipio_data$Data.Presentazione <- as.Date(municipio_data$Data.Presentazione)


municipio_data$Area.Tematica <- ifelse(municipio_data$Area.Tematica == "CASA E URBANISTICA", "ALTRI SERVIZI", municipio_data$Area.Tematica)  
municipio_data$Area.Tematica <- ifelse(municipio_data$Area.Tematica == "COMMERCIO E IMPRESA", "ALTRI SERVIZI", municipio_data$Area.Tematica)
municipio_data$Area.Tematica <- ifelse(municipio_data$Area.Tematica == "TURISMO", "ALTRI SERVIZI", municipio_data$Area.Tematica)
municipio_data$Area.Tematica <- ifelse(municipio_data$Area.Tematica == "DIRITTI E PARI OPPORTUNITÀ", "ALTRI SERVIZI", municipio_data$Area.Tematica)
municipio_data$Area.Tematica <- ifelse(municipio_data$Area.Tematica == "CULTURA", "ALTRI SERVIZI", municipio_data$Area.Tematica)

municipio_data <- subset(municipio_data, select = -c(Anno,Mese,Giorno, Municipio))

#table(municipio_data$Area.Tematica)

#Train-test split
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
                       month(matrice_dati$Data.Presentazione) == 12 & day(matrice_dati$Data.Presentazione) == 25 |
                       month(matrice_dati$Data.Presentazione) == 4 & day(matrice_dati$Data.Presentazione) == 25 |
                       month(matrice_dati$Data.Presentazione) == 8 & day(matrice_dati$Data.Presentazione) == 15 |
                       month(matrice_dati$Data.Presentazione) == 12 & (day(matrice_dati$Data.Presentazione) %in% c(24, 26, 31)) |
                       month(matrice_dati$Data.Presentazione) == 1 & (day(matrice_dati$Data.Presentazione) %in% c(1, 6)) |
                       month(matrice_dati$Data.Presentazione) == 6 & day(matrice_dati$Data.Presentazione) == 29 |
                       month(matrice_dati$Data.Presentazione) == 6 & day(matrice_dati$Data.Presentazione) == 2 |
                       month(matrice_dati$Data.Presentazione) == 5 & day(matrice_dati$Data.Presentazione) == 1 |
                       month(matrice_dati$Data.Presentazione) == 11 & day(matrice_dati$Data.Presentazione) == 1, 1, 0)


matrice_dati$POSTFESTIVO <- ifelse(month(matrice_dati$Data.Presentazione) == 4 & day(matrice_dati$Data.Presentazione) == 26 |
                                   month(matrice_dati$Data.Presentazione) == 12 & day(matrice_dati$Data.Presentazione) == 27 |
                                   month(matrice_dati$Data.Presentazione) == 8 & day(matrice_dati$Data.Presentazione) == 16 |
                                   month(matrice_dati$Data.Presentazione) == 1 & day(matrice_dati$Data.Presentazione) == 7 |
                                   month(matrice_dati$Data.Presentazione) == 6 & day(matrice_dati$Data.Presentazione) == 30 |  
                                   month(matrice_dati$Data.Presentazione) == 6 & day(matrice_dati$Data.Presentazione) == 3 |
                                   month(matrice_dati$Data.Presentazione) == 5 & day(matrice_dati$Data.Presentazione) == 2 |
                                   (weekdays(matrice_dati$Data.Presentazione) == "lunedì" & matrice_dati$FESTIVO == 0 )   |
                                   month(matrice_dati$Data.Presentazione) == 11 & day(matrice_dati$Data.Presentazione) == 2, 1,0 )

matrice_dati$MESE <- as.integer(format(matrice_dati$Data.Presentazione, "%m"))

matrice_dati <- matrice_dati %>%
  mutate(across(everything(), as.numeric))


ordered_columns <- order(names(matrice_dati))

#ordino le colonne in base all'ordine alfabetico
matrice_dati <- matrice_dati[, ordered_columns]

#matrice_dati <- subset(matrice_dati, select = -Data.Presentazione)
#################################
#DATI
#Segnalazioni
ts_unique <- data_filtered_municipi %>%
  group_by(Data.Presentazione, 
           Mese,
           Giorno,
           Anno,
           Municipio) %>%
  summarise(Numero_Segnalazioni = n())

municipio_dhr <- subset(ts_unique, Municipio== 5)

municipio_dhr_train <- municipio_dhr %>% filter(Data.Presentazione < as.Date("2023-01-01"))
municipio_dhr_test <- municipio_dhr %>% filter(Data.Presentazione >= as.Date("2023-01-01"))


municipio_dhr_train <- subset(municipio_dhr_train, select = -c(Mese, Giorno, Anno, Municipio))
municipio_dhr_train$Data.Presentazione <- as.Date(municipio_dhr_train$Data.Presentazione)

new_data <- data.frame(
  Numero_Segnalazioni = municipio_dhr_train$Numero_Segnalazioni
)
rownames(new_data) <- municipio_dhr_train$Data.Presentazione



#################################
#DHR NO REGRESSORS 
#RESULTS --> 
# sigma^2 = 0.264:  log likelihood = -403.5
# AIC=835.01   AICc=835.8   BIC=895.17
res1<-dhr(new_data,
          XREG = NULL,        # no indipendent variable
          Range = list(1:5, 1),       # Range of terms for Fourier's model
          #Frequency = c(24, 168),
          Frequency = c(365),         # Seasonality (dayly and weekly)
          Criteria = "aicc",          # Model selection criteria
          maxp = 5,                   # Max order of AR in auto.arima model 
          maxq = 5,                   # Max order of MA in auto.arima model
          maxd = 5) 

res1
##################################
#DHR 1 REGRESSOR --> ALTRI SERVIZI ---> è peggio!!!
#RESULTS --> 
# sigma^2 = 0.269:  log likelihood = -408.05
# AIC=846.1   AICc=847.01   BIC=910.55

res1<-dhr(new_data,
          XREG = as.matrix(matrice_dati$`ALTRI SERVIZI`),        
          Range = list(1:5, 1),   
          Frequency = c(365),         
          Criteria = "aicc",          
          maxp = 5,                    
          maxq = 5,                   
          maxd = 5) 

res1

####################################
#DHR 1 REGRESSOR --> AMBIENTE ---> è meglio!!!
#RESULTS --> 
# sigma^2 = 0.2215:  log likelihood = -355.8
# AIC=739.59   AICc=740.39   BIC=799.75
res1<-dhr(new_data,
          XREG = as.matrix(matrice_dati$AMBIENTE),        
          Range = list(1:5, 1),   
          Frequency = c(365),         
          Criteria = "aicc",          
          maxp = 5,                    
          maxq = 5,                   
          maxd = 5) 

res1
#########################################
#DHR 2 REGRESSORS --> AMBIENTE + FESTIVo ---> è ancora meglio!!!
#RESULTS --> 
# sigma^2 = 0.1675:  log likelihood = -278.85
# AIC=589.7   AICc=590.73   BIC=658.45
res1<-dhr(new_data,
          XREG = as.matrix(matrice_dati[,c(2,3)]),        
          Range = list(1:5, 1),   
          Frequency = c(365),         
          Criteria = "aicc",          
          maxp = 5,                    
          maxq = 5,                   
          maxd = 5) 

res1
#######################################
#DHR 3 REGRESSORS --> AMBIENTE + FESTIVO + RIFIUTI ---> è ancora meglio!!!
#RESULTS --> 
# sigma^2 = 0.1186:  log likelihood = -187.22
# AIC=396.45   AICc=396.94   BIC=443.74
res1<-dhr(new_data,
          XREG = as.matrix(matrice_dati[,c(2,3,4)]),        
          Range = list(1:5, 1),   
          Frequency = c(365),         
          Criteria = "aicc",          
          maxp = 5,                    
          maxq = 5,                   
          maxd = 5) 

res1

#######################################
#DHR 4 REGRESSORS --> AMBIENTE + FESTIVO + RIFIUTI + MESE --> peggio
#RESULTS --> 
# sigma^2 = 0.1177:  log likelihood = -182.56
# AIC=399.12   AICc=400.29   BIC=472.17
res1<-dhr(new_data,
          XREG = as.matrix(matrice_dati[,c(2,3,4,5)]),        
          Range = list(1:5, 1),   
          Frequency = c(365),         
          Criteria = "aicc",          
          maxp = 5,                    
          maxq = 5,                   
          maxd = 5) 

res1
######################################
#DHR 4 REGRESSORS --> AMBIENTE + FESTIVO + RIFIUTI + TRASPORTI --> poco meglio 
#RESULTS --> 
# sigma^2 = 0.1187:  log likelihood = -186.82
# AIC=397.65   AICc=398.23   BIC=449.23
res1<-dhr(new_data,
          XREG = as.matrix(matrice_dati[,c(2,3,4,6)]),        
          Range = list(1:5, 1),   
          Frequency = c(365),         
          Criteria = "aicc",          
          maxp = 5,                    
          maxq = 5,                   
          maxd = 5) 

res1
#######################################
#DHR 5 REGRESSORS --> AMBIENTE + FESTIVO + RIFIUTI + TRASPORTI + CITTà --> molto meglio! 
#RESULTS --> 
# sigma^2 = 0.1055:  log likelihood = -151.97
# AIC=341.93   AICc=343.38   BIC=423.58
res1<-dhr(new_data,
          XREG = as.matrix(matrice_dati[,c(2,3,4,6,7)]),        
          Range = list(1:5, 1),   
          Frequency = c(365),         
          Criteria = "aicc",          
          maxp = 5,                    
          maxq = 5,                   
          maxd = 5) 

res1
######################################
#DHR 6 REGRESSORS --> AMBIENTE + FESTIVO + RIFIUTI + TRASPORTI + CITTà + POSTFESTIVO --> pochissimo peggio! 
#RESULTS --> 
# sigma^2 = 0.1055:  log likelihood = -151.97
# AIC=341.93   AICc=343.38   BIC=423.58
res1<-dhr(new_data,
          XREG = as.matrix(matrice_dati[,c(2,3,4,6,7,8)]),        
          Range = list(1:5, 1),   
          Frequency = c(365),         
          Criteria = "aicc",          
          maxp = 5,                    
          maxq = 5,                   
          maxd = 5) 

res1
##############################
#DHR 6 REGRESSORS --> AMBIENTE + FESTIVO + RIFIUTI + TRASPORTI + CITTà + SERVIZI MUNICIPALI --> pochissimo meglio! 
#RESULTS --> 
# sigma^2 = 0.1048:  log likelihood = -149.7
# AIC=339.4   AICc=341.01   BIC=425.34
res1<-dhr(new_data,
          XREG = as.matrix(matrice_dati[,c(2,3,4,6,7,9)]),        
          Range = list(1:5, 1),   
          Frequency = c(365),         
          Criteria = "aicc",          
          maxp = 5,                    
          maxq = 5,                   
          maxd = 5) 

res1
##############################
######MODELLO MIGLIORE########
##############################
#DHR 7 REGRESSORS --> AMBIENTE + FESTIVO + RIFIUTI + TRASPORTI + CITTà + SERVIZI MUNICIPALI + POLIZIA--> meglio! 
#RESULTS --> 
# sigma^2 = 0.08042:  log likelihood = -74.82
# AIC=197.65   AICc=199.96   BIC=300.82
res1<-dhr(new_data,
          XREG = as.matrix(matrice_dati[,c(2,3,4,6,7,9,10)]),        
          Range = list(1:5, 1),   
          Frequency = c(365),         
          Criteria = "aicc",          
          maxp = 5,                    
          maxq = 5,                   
          maxd = 5) 

res1


checkresiduals(res1)
tsdiag(res1, gof.lag = 24)


#Forecast -----------------------------

#FUNZIONE FORECAST + PLOT 
pred1<-fc(Frequency = c(365), 
         XREG_test = as.matrix(matrice_dati[,c(2,3,4,6,7,9,10)]),
         #XREG_test = NULL, 
         h = 500,
          Fit = res1, 
          Data = new_data) 

plot(pred1)
str(pred1)


head(municipio_dhr_test)

pred <- pred1$fitted[0:90]
pred_lower <- pred1$lower[0:90,2]
pred_upper <- pred1$upper[0:90,2]


plot(pred, type = "l", ylim = range(c(pred_lower,pred_upper)), main = "Predizioni Cumulative vs Real Data MUNICIPIO V GENNAIO-MARZO 2023",
     xlab = "N Giorno", ylab = "N Complaints")
polygon(c(1:length(pred), length(pred):1), c(pred_upper, rev(pred_lower)), col = "grey", border = NA)
lines(pred, type = "l", col="blue")
lines(municipio_dhr_test$Numero_Segnalazioni, type = "l")
legend("topright", legend = c("Predetto", "Osservato","CI 95%"), col = c("blue", "black", "grey"), lty = 1)








#commento da aggiungere:
#magari lo abbiamo confuso noi con 1/6 gennaio che abbiamo inserito noi che sono festivi


##################################
##########AREA TEMATICA###########
##################################









