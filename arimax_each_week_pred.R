# For foecasting mars and competitor prices

library(TSA)
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
final_model_result <- data.frame()

#storing the list of unique ZREPs 
list_of_keys<-unique(data$ZREP_ID_ALL)

ads <- data

ads <- ads %>% filter( !is.na(AUP))



for(key in list_of_keys){
  
  print(key)
  curr_zrep <- ads %>% filter(ZREP_ID_ALL == key)
  
  if( nrow(curr_zrep) > 83){
    
    tr_in = 48
    ts_in = 47
    
    Model_Results <- data.frame()
    
    while (tr_in > 0 & ts_in > 0) {
      
      tr_in = tr_in - 1
      
      ts_in = ts_in - 1 
      
      Current_results<-data.frame(matrix(nrow = 1, ncol = 3 , data = NA))
      colnames(Current_results)<- c("ZREP","AIC","Forecasts")
      
      
      #Dividing the data into train and test 
      curr_df_train <- curr_zrep[1:(nrow(curr_zrep)-tr_in),]
      curr_df_test <- curr_zrep[(nrow(curr_zrep)-(ts_in)):(nrow(curr_zrep)-(ts_in)),]
      
      tryCatch(
        {
          
          curr_ts <- as.ts(curr_df_train$order_qty , frequency=52)
          curr_test_ts <- as.ts(curr_df_test$order_qty , frequency=48)
          
          #adding the external events variable 
          regr_curr_train<- curr_df_train[,c("holiday_flag","AUP")]
          
          #running the model
          curr_arima<- arimax(curr_ts,xreg = regr_curr_train)
          
          # summary(curr_arima)
          
          #storing zrep and aic after running arimax 
          Current_results$ZREP <- key
          Current_results$AIC <- curr_arima$aic
          Current_results$holiday_flag_coef <- curr_arima$coef["holiday_flag"]
          Current_results$AUP_coef <- curr_arima$coef["AUP"]
          
          
          #FORECASTING FUTURE VALUES 
          regr_forecast <- curr_df_test[,c("holiday_flag","AUP")]
          # regr_forecast <- as.matrix(regr_forecast)
          
          #curr_forecast<-forecast(curr_arima,h=26)
          curr_forecast <- predict(curr_arima, newxreg = regr_forecast)
          Current_results$Forecasts <- curr_forecast$pred
          
          #merging with main data to calculate accuracy and mape 
          final_test_ads <- cbind(Current_results, curr_df_test)
          
          final_test_ads$MAPE <- abs(final_test_ads$Forecasts - final_test_ads$order_qty)/final_test_ads$order_qty
          
          #CALCULATING ACCURACY
          final_test_ads$acc <- 1 - (abs(final_test_ads$Forecasts - final_test_ads$order_qty)/final_test_ads$Forecasts)
          
        },
        error=function(e){
          print("error")
        }
      )
      Model_Results <- bind_rows(Model_Results,final_test_ads)
    }
    final_model_result <- rbind(final_model_result, Model_Results)
  }else{
    print("skipped")
  }
}

# Model_Results$Forecasts<-round(Model_Results$Forecasts,1)
write.csv(final_model_result, file = "~/Mars/Enigma - Canada/Model/Different Model Results/Arimax_each_week.csv", row.names = F)



