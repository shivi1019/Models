install.packages("fpp2")

#moving average 

#loading libraries 
library(dplyr)
library(tidyverse)      # data manipulation and visualization
library(lubridate)      # easily work with dates and times
library(fpp2)           # working with time series data
library(zoo)            # working with time series data

#reading files 
order_shipment <- read.csv("~/Mars/Enigma - Canada/Data/PROCESS/Holiday/OD_HolidayFlagsFinal16171819.csv", stringsAsFactors = F)
length(unique(order_shipment$ZREP))

# zrep_list <- read.csv("~/Mars/Enigma - Canada/Data/RAW/selected_zreps_83.csv", stringsAsFactors = F)

#filter for top 83 zreps used in phase 1 
# ods_83 <- order_shipment %>% filter(ZREP %in% zrep_list$ZREP)

#filtering for year < 2018
# ods_83 <- ods_83 %>% filter(req_delivery_date_mars_year %in% c(2017,2018))

ods_83 <- order_shipment

ods_83 <- ods_83 %>% filter(req_delivery_date_mars_year %in% c(2017,2018,2019))


zrep <- unique(ods_83$ZREP)

all_results <- data.frame()

rollingMean <- function(col, x){
  rollmean(col, k = x, fill = NA, align = 'right')
}

for(each_zrep in zrep){
  model_ads <- ods_83 %>% filter( ZREP %in% each_zrep)
  
  model_ads$week_starting_date <- as.Date(model_ads$week_starting_date, format = "%Y-%m-%d")
  model_ads <- model_ads[order(model_ads$week_starting_date),]
  
  model_ads_zrep <- model_ads %>% select(ZREP,order_qty ,week_starting_date) %>% mutate(
    od_mav_01 = rollingMean( order_qty, 5),
    od_mav_02 = rollingMean( order_qty, 7),
    od_mav_03 = rollingMean( order_qty, 9),
    od_mav_04 = rollingMean( order_qty, 11),
    od_mav_05 = rollingMean( order_qty, 52))
  
  all_results <- bind_rows(all_results, model_ads_zrep)
  
}


zrep1 <- all_results %>% filter( ZREP == 305899)

table(zrep1$week_starting_date)

# zrep1$week_starting_date <- as.Date(zrep1$week_starting_date, format = "%Y-%m-%d")


all_results



zrep1 %>%
  gather(metric, value, c(order_qty,od_mav_01,od_mav_02,od_mav_03,od_mav_04,od_mav_05)) %>%
  ggplot(aes(week_starting_date, value, color = metric)) +
  geom_line()

tail(zrep1, 12)

#Calculating error 
mape_acc_all_Zreps <- all_results %>%
  gather(metric, value, c(od_mav_01,od_mav_02,od_mav_03,od_mav_04,od_mav_05)) %>%
  group_by(metric, ZREP,week_starting_date) %>%
  summarise(MSE = mean((order_qty - value)^2, na.rm = TRUE),
            MAPE = mean(abs((order_qty - value)/order_qty * 100), na.rm = TRUE),
            Accuracy = mean((1 - abs(order_qty - value)/value)*100, na.rm = T))

vf <- all_results[,c("ZREP","week_starting_date","order_qty", "od_mav_05")]

vf <- vf %>% mutate(
  MAPE_MA = abs(order_qty - od_mav_05)/order_qty,
  acc_MA = (1 - abs(order_qty - od_mav_05)/od_mav_05)
)



write.csv(vf, file = "~/Mars/Enigma - Canada/Model/Different Model Results/MovingAverage.csv", row.names = F)








