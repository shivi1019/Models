# For foecasting mars and competitor prices

# library(TSA)
library(dplyr)
library(tseries)
library(forecast)
library(lubridate)



#Getting the datasets for usage
data <- read.csv("~/Mars/Enigma - Canada/Data/PROCESS/Order/treated_order_data.csv", header = T, stringsAsFactors = F)


# Creating indexes around the time series 
# IN case not ordered use this code ----ts_sub<-ts_sub[order(ts_sub$adate),]

str(data$Date_ALL)

data<-data[order(data$Date_ALL),]

#creating empty dataframe to store results 
Model_Results<-data.frame()



#storing the list of unique ZREPs 
list_of_keys<-unique(data$ZREP_ID_ALL)

ads <- data

ads <- ads %>% filter( !is.na(AUP))

key = "328273"

for(key in list_of_keys){
  
  print(key)
  
  Current_results<-data.frame(matrix(nrow = 48, ncol = 3 , data = NA))
  colnames(Current_results)<- c("ZREP","AIC","Forecasts")
  
  curr_zrep <- ads %>% filter(ZREP_ID_ALL == key)
  
  #Since the p-value is 0.96220, the correct interpretation is "There is 96.22% that my model has unit root", 
  #which is equivalent to saying "There is 96.22% that my process is non stationary". Therefore, you cannot reject H0
  # adf.test(curr_zrep$treated_order_qty)
  
  #Dividing the data into train and test 
  curr_df_train <- curr_zrep[1:(nrow(curr_zrep)-48),]
  curr_df_test <- curr_zrep[(nrow(curr_zrep)-47):nrow(curr_zrep),]
  
  tryCatch(
    {
      
      curr_ts <- as.ts(curr_df_train$order_qty , frequency=52)
      curr_test_ts <- as.ts(curr_df_test$order_qty , frequency=48)
      
      #adding the external events variable 
      regr_curr_train<- curr_df_train[,c("holiday_flag","AUP")]
      
      #running the model
      # curr_arima<-auto.arima(curr_ts,xreg=regr_curr_train)
      
      curr_arima<- auto.arima(curr_ts, max.p = 10, max.d = 2, max.q = 10, start.p = 1,start.q = 1, xreg=regr_curr_train, seasonal = T)
      
      # summary(curr_arima)
      
      #storing zrep and aic after running arimax 
      Current_results$ZREP <- key
      Current_results$AIC <- curr_arima$aic
      Current_results$holiday_flag_coef <- curr_arima$coef["holiday_flag"]
      Current_results$AUP_coef <- curr_arima$coef["AUP"]
      Current_results$MAPE <- accuracy(curr_arima)[5]
      
      #FORECASTING FUTURE VALUES 
      regr_forecast <- curr_df_test[,c("holiday_flag","AUP")]
      # regr_forecast <- as.matrix(regr_forecast)
      
      #curr_forecast<-forecast(curr_arima,h=26)
      curr_forecast <- predict(curr_arima, newxreg = regr_forecast, h = 48)
      Current_results$Forecasts <- curr_forecast$pred
      
      #merging with main data to calculate accuracy and mape 
      final_test_ads <- cbind(Current_results, curr_df_test)
      
      #CALCULATING ACCURACY
      final_test_ads$acc <- 1 - (abs(final_test_ads$Forecasts - final_test_ads$order_qty)/final_test_ads$Forecasts)
      
      Model_Results <- bind_rows(Model_Results,final_test_ads)
      
    },
    error=function(e){
      print("error")
    }
  )
}
}


# Model_Results$Forecasts<-round(Model_Results$Forecasts,1)
write.csv(Model_Results, file = "~/Mars/Enigma - Canada/Model/Different Model Results/Auto arima results.csv", row.names = F)



