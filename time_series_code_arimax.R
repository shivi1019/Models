# For foecasting mars and competitor prices

library(TSA)
library(dplyr)
library(tseries)
library(forecast)
library(lubridate)



#Getting the datasets for usage
data_all <- read.csv("~/Mars/Enigma - Canada/Data/PROCESS/Holiday/OD_HolidayFlagsFinal.csv", header = T, stringsAsFactors = F)
data <- unique(data_all[,c("ZREP","week_starting_date","order_qty","shipment_qty","req_delivery_date_mars_year","holiday_flag")])

# Creating indexes around the time series 
# IN case not ordered use this code ----ts_sub<-ts_sub[order(ts_sub$adate),]

str(data$week_starting_date)
data$week_starting_date <- as.Date(data$week_starting_date,format = "%Y-%m-%d")

data<-data[order(data$week_starting_date),]

#creating empty dataframe to store results 
Model_Results<-data.frame(matrix(ncol = 3, data = NA))
colnames(Model_Results)<- c("ZREP","AIC","Forecasts")


#storing the list of unique ZREPs 
list_of_keys<-unique(data$ZREP)

ads <- data
# key = "305899"

for(key in list_of_keys){
  
  print(key)
  
  Current_results<-data.frame(matrix(nrow=52, ncol =3 , data = NA))
  colnames(Current_results)<- c("ZREP","AIC","Forecasts")
  
  curr_zrep <- ads %>% filter(ZREP == key)
  
  #Since the p-value is 0.96220, the correct interpretation is "There is 96.22% that my model has unit root", 
  #which is equivalent to saying "There is 96.22% that my process is non stationary". Therefore, you cannot reject H0
  adf.test(curr_zrep$order_qty)
  
  #Dividing the data into train and test 
  curr_df_train <- curr_zrep[1:(nrow(curr_zrep)-52),]
  curr_df_test <- curr_zrep[(nrow(curr_zrep)-51):nrow(curr_zrep),]
  
  curr_ts <- as.ts(curr_df_train$order_qty , frequency=52)
  curr_test_ts <- as.ts(curr_df_test$order_qty , frequency=52)
  
  #adding the external events variable 
  regr_curr_train<-as.vector(curr_df_train$holiday_flag)
  
  #running the model
  # curr_arima<-auto.arima(curr_ts,xreg=regr_curr_train)
  
  curr_arima<- arimax(curr_ts,xreg=regr_curr_train, method = "ML")
  
  # summary(curr_arima)
  #storing zrep and aic after running arimax 
  Current_results$ZREP <- key
  Current_results$AIC <- curr_arima$aic
  Current_results$holiday_flag_coef <- curr_arima$coef["xreg"]
  
  #FORECASTING FUTURE VALUES 
  regr_forecast <- curr_df_test$holiday_flag
  
  regr_forecast_vec<-as.vector(regr_forecast)
  
  #curr_forecast<-forecast(curr_arima,h=26)
  curr_forecast <- predict(curr_arima, newxreg = regr_forecast_vec , h=52)
  Current_results$Forecasts<-curr_forecast$pred
  
  #merging with main data to calculate accuracy and mape 
  final_test_ads <- cbind(Current_results, curr_df_test)
  
  #CALCULATING ACCURACY
  final_test_ads$acc <- 1 - (abs(final_test_ads$Forecasts - final_test_ads$order_qty)/final_test_ads$Forecasts)
  #CALCULATING MAPE 
  final_test_ads$MAPE <- (abs(final_test_ads$Forecasts - final_test_ads$order_qty)/final_test_ads$order_qty)
  
  Model_Results <- bind_rows(Model_Results,final_test_ads)
  
}

Model_Results<- Model_Results[-1,]
# Model_Results$Forecasts<-round(Model_Results$Forecasts,1)

write.csv(Model_Results, file = "~/Mars/Enigma - Canada/Model/ArimaxHolidayResults_usingTSA.csv", row.names = F)



