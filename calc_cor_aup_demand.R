
#calculating correlation with aup and demand with 1 week lag 
pos_data <- readxl::read_excel("~/Mars/Enigma - Canada/Model/ArimaxHolidayResults_AUP.xlsx", sheet = "ArimaxHolidayResults_AUP" )


pos_1lag <- unique(pos_data[,c("ZREP_ID_ALL","Date_ALL","order_qty")])

pos_aup <-  unique(pos_data[,c("ZREP_ID_ALL","Date_ALL","AUP")])


#Merging the filtered holidays with order shipment data 
pos_w_lag1 <- sqldf::sqldf("SELECT * 
  FROM pos_1lag LEFT JOIN pos_aup 
  ON pos_1lag.ZREP_ID_ALL = pos_aup.ZREP_ID_ALL AND 
                             pos_1lag.Date_ALL = pos_aup.Date_ALL - 1 ")




cor_each_zrep <- pos_w_lag1 %>% group_by( ZREP_ID_ALL) %>% summarise( cor_score = cor(order_qty, AUP))

write.csv(cor_each_zrep, "~/Mars/Enigma - Canada/Model/cor_each_zrep_w1lag.csv", row.names = F)

sum(is.na(pos_w_lag1))

pos_w_lag1[is.na(pos_w_lag1)] <- 0




