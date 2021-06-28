setwd('C:\\Users\\souradip.c\\Work\\CSAT Driver Model\\Final Driver Model Datasets')

library(dplyr)
library(glmnet)
library(BBmisc)
library(corrplot)
library(regclass)
library(reshape2)

#################### Reading and Manipulating Data ####################
df_csat_drivers_post_ld = read.csv('csat_drivers_post_lockdown.csv')

#creating channel level share
df_csat_drivers_post_ld$email_share = ifelse(df_csat_drivers_post_ld$channel_name == 'Email' | df_csat_drivers_post_ld$channel_name == 'CSS Email', 
                                             df_csat_drivers_post_ld$responses, 0)
df_csat_drivers_post_ld$chat_share = ifelse(df_csat_drivers_post_ld$channel_name == 'Chat', df_csat_drivers_post_ld$responses, 0)
df_csat_drivers_post_ld$voice_share = ifelse(df_csat_drivers_post_ld$channel_name == 'Inbound' | df_csat_drivers_post_ld$channel_name == 'Outbound', 
                                        df_csat_drivers_post_ld$responses, 0)

# Creating analytic_business_unit Level Shares
df_csat_drivers_post_ld$BGM_share = ifelse(df_csat_drivers_post_ld$analytic_business_unit == 'BGM', df_csat_drivers_post_ld$unit_count, 0)
df_csat_drivers_post_ld$Electronics_share = ifelse(df_csat_drivers_post_ld$analytic_business_unitiness_unit == 'Electronics', df_csat_drivers_post_ld$unit_count, 0)
df_csat_drivers_post_ld$Furniture_share = ifelse(df_csat_drivers_post_ld$analytic_business_unit == 'Furniture', df_csat_drivers_post_ld$unit_count, 0)
df_csat_drivers_post_ld$Home_share = ifelse(df_csat_drivers_post_ld$analytic_business_unit == 'Home', df_csat_drivers_post_ld$unit_count, 0)
df_csat_drivers_post_ld$Large_share = ifelse(df_csat_drivers_post_ld$analytic_business_unit == 'Large', df_csat_drivers_post_ld$unit_count, 0)
df_csat_drivers_post_ld$LifeStyle_share = ifelse(df_csat_drivers_post_ld$analytic_business_unit == 'LifeStyle', df_csat_drivers_post_ld$unit_count, 0)

df_csat_drivers_post_ld$Mobile_share = ifelse(df_csat_drivers_post_ld$analytic_business_unit == 'Mobile', df_csat_drivers_post_ld$unit_count, 0)

# Creating OCR responses
df_csat_drivers_post_ld$ocr_count = ifelse(df_csat_drivers_post_ld$ocr_flag == 1, df_csat_drivers_post_ld$responses, 0)

fact_num <- c('ocr_flag',
              'responses',
              'csat_rate_count',
              'reopen_count',
              'return_requests_items',
              'courier_return_items',
              'rvp_approval_count',
              'ss_approval_count',
              'replacement_approval',
              'unit_count',
              'ekl_units',
              'fbf_units',
              'fad2_o2dunits',
              'fad4_o2dunits',
              'fad4pl_o2dunits',
              'nfad4_o2dunits',
              'nfad6_o2dunits',
              'nfad8_o2dunits',
              'nfad8pl_o2dunits',
              'fad2_slaunits',
              'fad4_slaunits',
              'fad6_slaunits',
              'fad8_slaunits',
              'fad10_slaunits',
              'fad10plus_slaunits',
              'nfad4_slaunits',
              'nfad6_slaunits',
              'nfad8_slaunits',
              'nfad10_slaunits',
              'nfad10plus_slaunits',
              'sp_less_than_200',
              'sp_between_500_1k',
              'sp_between_1k_2k',
              'sp_between_2k_10k',
              'sp_between_10k_20k',
              'sp_greater_than_20k',
              'sr_less_than_3_3units',
              'sr_less_than_4units',
              'sr_greater_than_4units',
              'cancellation_units',
              'cod_units',
              'fabreachunits',
              'nfabreachunits',
              'promotional_discount',
              'ndd_sdd',
              'avg_sla',
              'avg_o2d',
              'avg_selling_price',
              'order_qty',
              'customer_cancellation_qty',
              'ivrs_qty',
              'fraud_tns_post_order_cancel_qty',
              'fk_group_cancel_qty',
              'seller_cancel_qty',
              'fk_credit_fail',
              'fk_credit_trans',
              'credit_fail',
              'credit_trans',
              'debit_fail',
              'debit_trans',
              'debit_emi_fail',
              'debit_emi_trans',
              'fk_finance_fail',
              'fk_finance_trans',
              'emi_fail',
              'emi_trans',
              'net_fail',
              'net_trans',
              'phonepe_fail',
              'phonepe_trans',
              'upi_collect_fail',
              'upi_collect_trans',
              'upi_intent_fail',
              'upi_intent_trans',
              'cod_fail',
              'cod_trans',
              'frm_count',
              'e2e_count',
              'customer_threads',
              'note_threads',
              'rule_reponse_threads',
              'staff_account_threads',
              'rnr_threads',
              'threads'
              )

df_csat_drivers_post_ld[,fact_num] <- apply(df_csat_drivers_post_ld[,fact_num], 2, function(x) as.numeric(as.character(x)))
df_csat_drivers_post_ld[,fact_num][is.na(df_csat_drivers_post_ld[,fact_num])] <- 0

fact_num <- c('pcd',
              'resolution',
              'soft_skills',
              'communication_proficiency',
              'compassion',
              'composure',
              'customer_centricity',
              'agg_four_c',
              'tenurity')

df_csat_drivers_post_ld[,fact_num] <- apply(df_csat_drivers_post_ld[,fact_num], 2, function(x) as.numeric(as.character(x)))

#################### Enterprise Level CSAT Driver Model Data ####################

df_enterprise_model_post_ld = df_csat_drivers_post_ld %>%
  filter(response_week>=20 & response_week <= 30) %>% 
  group_by(response_date) %>%
  summarise(csat_rate_count = sum(csat_rate_count)/sum(responses),
            ocr_count = sum(ocr_count)/sum(responses),
            reopen_count = sum(reopen_count)/sum(responses),
            ekl_units = sum(ekl_units)/sum(unit_count),
            fbf_units = sum(fbf_units)/sum(unit_count),
            fad2_o2dunits = sum(fad2_o2dunits)/sum(unit_count),
            fad4_o2dunits = sum(fad4_o2dunits)/sum(unit_count),
            fad4pl_o2dunits = sum(fad4pl_o2dunits)/sum(unit_count),
            nfad4_o2dunits = sum(nfad4_o2dunits)/sum(unit_count),
            nfad6_o2dunits = sum(nfad6_o2dunits)/sum(unit_count),
            nfad8_o2dunits = sum(nfad8_o2dunits)/sum(unit_count),
            nfad8pl_o2dunits = sum(nfad8pl_o2dunits)/sum(unit_count),
            fad2_slaunits = sum(fad2_slaunits)/sum(unit_count),
            fad4_slaunits = sum(fad4_slaunits)/sum(unit_count),
            fad6_slaunits = sum(fad6_slaunits)/sum(unit_count),
            fad8_slaunits = sum(fad8_slaunits)/sum(unit_count),
            fad10_slaunits = sum(fad10_slaunits)/sum(unit_count),
            fad10plus_slaunits = sum(fad10plus_slaunits)/sum(unit_count),
            nfad4_slaunits = sum(nfad4_slaunits)/sum(unit_count),
            nfad6_slaunits = sum(nfad6_slaunits)/sum(unit_count),
            nfad8_slaunits = sum(nfad8_slaunits)/sum(unit_count),
            nfad10_slaunits = sum(nfad10_slaunits)/sum(unit_count),
            nfad10plus_slaunits = sum(nfad10plus_slaunits)/sum(unit_count),
            sp_less_than_200 = sum(sp_less_than_200)/sum(unit_count),
            sp_between_500_1k = sum(sp_between_500_1k)/sum(unit_count),
            sp_between_1k_2k = sum(sp_between_1k_2k)/sum(unit_count),
            sp_between_2k_10k = sum(sp_between_2k_10k)/sum(unit_count),
            sp_between_10k_20k = sum(sp_between_10k_20k)/sum(unit_count),
            sp_greater_than_20k = sum(sp_greater_than_20k)/sum(unit_count),
            sr_less_than_3_3units = sum(sr_less_than_3_3units)/sum(unit_count),
            sr_less_than_4units = sum(sr_less_than_4units)/sum(unit_count),
            sr_greater_than_4units = sum(sr_greater_than_4units)/sum(unit_count),
            cancellation_units = sum(cancellation_units)/sum(unit_count),
            cod_units = sum(cod_units)/sum(unit_count),
            fabreachunits = sum(fabreachunits)/sum(unit_count),
            nfabreachunits = sum(nfabreachunits)/sum(unit_count),
            promotional_discount = sum(promotional_discount)/sum(unit_count),
            ndd_sdd = sum(ndd_sdd)/sum(unit_count),
            avg_sla = mean(avg_sla),
            avg_o2d = mean(avg_o2d),
            avg_selling_price = mean(avg_selling_price),
            BGM_share = sum(BGM_share)/sum(unit_count),
            Electronics_share = sum(Electronics_share)/sum(unit_count),
            Furniture_share = sum(Furniture_share)/sum(unit_count),
            Home_share = sum(Home_share)/sum(unit_count),
            Large_share = sum(Large_share)/sum(unit_count),
            LifeStyle_share = sum(LifeStyle_share)/sum(unit_count),
            Mobile_share = sum(Mobile_share)/sum(unit_count),
            return_requests_items = sum(return_requests_items)/sum(unit_count),
            rvp_approval_count = sum(rvp_approval_count)/sum(unit_count),
            ss_approval_count = sum(ss_approval_count)/sum(unit_count),
            replacement_approval = sum(replacement_approval)/sum(unit_count),
            email_share = sum(email_share)/sum(responses),
            chat_share = sum(chat_share)/sum(responses),
            voice_share = sum(voice_share)/sum(responses),
            customer_cancellation_share = sum(customer_cancellation_qty)/sum(order_qty),
            fraud_tns_post_order_cancel_share = sum(fraud_tns_post_order_cancel_qty)/sum(order_qty),
            fk_group_cancel_share = sum(fk_group_cancel_qty)/sum(order_qty),
            frm_share = sum(frm_count)/sum(responses),
            e2e_share = sum(e2e_count)/sum(responses),
            customer_threads = sum(customer_threads)/sum(threads),
            note_threads = sum(note_threads)/sum(threads),
            rule_reponse_threads = sum(rule_reponse_threads)/sum(threads),
            staff_account_threads = sum(staff_account_threads)/sum(threads),
            rnr_threads = sum(rnr_threads)/sum(threads),
            pcd = mean(pcd, na.rm = T),
            resolution = mean(resolution, na.rm = T),
            soft_skills = mean(soft_skills, na.rm = T),
            communication_proficiency = mean(communication_proficiency, na.rm = T),
            compassion = mean(compassion, na.rm = T),
            composure = mean(composure, na.rm = T),
            customer_centricity = mean(customer_centricity, na.rm = T),
            agg_four_c = mean(agg_four_c, na.rm = T),
            tenurity = mean(tenurity, na.rm = T)
            )

write.csv(df_enterprise_model_post_ld, 'csat_enterprise_model_post_ld.csv')

df_enterprise_model_post_ld <- df_enterprise_model_post_ld %>% select(-response_date, -pcd, -resolution,
                                                                      -soft_skills, -compassion, -composure,
                                                                      -tenurity, -agg_four_c, -communication_proficiency,
                                                                      -customer_centricity)


#################### Fitting the model to find coefficients ####################
lin_mod <- lm(csat_rate_count ~ ., data = df_enterprise_model_post_ld)
summary(lin_mod)

##### Alias to find perfect collinearity
alias(lin_mod)

df_enterprise_model_post_ld <- df_enterprise_model_post_ld %>% select(-sr_less_than_3_3units, -sr_less_than_4units,
                                                                      -sr_greater_than_4units, -cancellation_units, -avg_o2d)

lin_mod <- lm(csat_rate_count ~ ., data = df_enterprise_model_post_ld)
summary(lin_mod)

##### VIF
count=0
col_len = length(colnames(df_enterprise_model_post_ld))

for (len in seq(1, col_len)) {
  
  lin_mod <- lm(csat_rate_count ~ ., data = df_enterprise_model_post_ld)
  summary(lin_mod)
  
  df_vif = data.frame(VIF(lin_mod))
  
  df_vif <- tibble::rownames_to_column(df_vif, "Variable")
  
  max_vif <- df_vif %>% filter(VIF.lin_mod. >= 25) %>% 
    filter(VIF.lin_mod. == max(df_vif$VIF.lin_mod.)) %>% select(Variable)
  
  print(max_vif)
  
  df_enterprise_model_post_ld <- df_enterprise_model_post_ld %>% select(-max_vif$Variable[1])
  count=count+1
  print(count+1)
}

lin_mod <- lm(csat_rate_count ~ ., data = df_enterprise_model_post_ld)
summary(lin_mod)

########## Random Forest Important Features ##########

rf <- randomForest(
  csat_rate_count ~ .,
  data=df_enterprise_model_post_ld 
)

feat_imp = data.frame(rf$importance)

feat_imp <- tibble::rownames_to_column(feat_imp, "Variable")

low_imp_variable <- feat_imp %>% 
  filter(IncNodePurity < quantile(feat_imp$IncNodePurity, probs = 0.25)) %>% select(Variable)

df_enterprise_model_post_ld <- df_enterprise_model_post_ld %>% select(-c(low_imp_variable$Variable))

lin_mod <- lm(csat_rate_count ~ ., data = df_enterprise_model_post_ld)
summary(lin_mod)

########## GLMNet ##########

x <- model.matrix(csat_rate_count~., df_enterprise_model_post_ld)[,-1]

colnames(x) <- colnames(x)

y <- df_enterprise_model_post_ld$csat_rate_count

##### Finding the best lambda
cv <- cv.glmnet(x, y, alpha = 0, standardize=T)

plot(cv)

##### Getting graphs of important variables
elasti_net <- glmnet(x, y, alpha = 0, standardize = T)


plot(elasti_net, xvar = "lambda", label = TRUE)


##### Fitting with the best lambda found
elasti_net <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
coef(elasti_net)

y_pred_lasso_train <- predict(elasti_net, x)
mape_train <- abs(y_pred_lasso_train - df_enterprise_model_post_ld$csat_rate_count)/df_enterprise_model_post_ld$csat_rate_count*100

mean(mape_train)

rss <- sum((y_pred_lasso_train - df_enterprise_model_post_ld$csat_rate_count) ^ 2)  ## residual sum of squares
tss <- sum((df_enterprise_model_post_ld$csat_rate_count - mean(df_enterprise_model_post_ld$csat_rate_count)) ^ 2)  ## total sum of squares
1 - ((1- (1 - rss/tss))*(nrow(df_enterprise_model_post_ld)-1))/(nrow(df_enterprise_model_post_ld) - 24 - 1)

#################### Predicting for Weeks ####################

df_enterprise_model_post_ld_week <- df_csat_drivers_post_ld %>%
  filter(response_week <= 30) %>% 
  group_by(response_week) %>%
  summarise(csat_rate_count = sum(csat_rate_count)/sum(responses),
            ocr_count = sum(ocr_count)/sum(responses),
            reopen_count = sum(reopen_count)/sum(responses),
            ekl_units = sum(ekl_units)/sum(unit_count),
            fbf_units = sum(fbf_units)/sum(unit_count),
            fad2_o2dunits = sum(fad2_o2dunits)/sum(unit_count),
            fad4_o2dunits = sum(fad4_o2dunits)/sum(unit_count),
            fad4pl_o2dunits = sum(fad4pl_o2dunits)/sum(unit_count),
            nfad4_o2dunits = sum(nfad4_o2dunits)/sum(unit_count),
            nfad6_o2dunits = sum(nfad6_o2dunits)/sum(unit_count),
            nfad8_o2dunits = sum(nfad8_o2dunits)/sum(unit_count),
            nfad8pl_o2dunits = sum(nfad8pl_o2dunits)/sum(unit_count),
            fad2_slaunits = sum(fad2_slaunits)/sum(unit_count),
            fad4_slaunits = sum(fad4_slaunits)/sum(unit_count),
            fad6_slaunits = sum(fad6_slaunits)/sum(unit_count),
            fad8_slaunits = sum(fad8_slaunits)/sum(unit_count),
            fad10_slaunits = sum(fad10_slaunits)/sum(unit_count),
            fad10plus_slaunits = sum(fad10plus_slaunits)/sum(unit_count),
            nfad4_slaunits = sum(nfad4_slaunits)/sum(unit_count),
            nfad6_slaunits = sum(nfad6_slaunits)/sum(unit_count),
            nfad8_slaunits = sum(nfad8_slaunits)/sum(unit_count),
            nfad10_slaunits = sum(nfad10_slaunits)/sum(unit_count),
            nfad10plus_slaunits = sum(nfad10plus_slaunits)/sum(unit_count),
            sp_less_than_200 = sum(sp_less_than_200)/sum(unit_count),
            sp_between_500_1k = sum(sp_between_500_1k)/sum(unit_count),
            sp_between_1k_2k = sum(sp_between_1k_2k)/sum(unit_count),
            sp_between_2k_10k = sum(sp_between_2k_10k)/sum(unit_count),
            sp_between_10k_20k = sum(sp_between_10k_20k)/sum(unit_count),
            sp_greater_than_20k = sum(sp_greater_than_20k)/sum(unit_count),
            sr_less_than_3_3units = sum(sr_less_than_3_3units)/sum(unit_count),
            sr_less_than_4units = sum(sr_less_than_4units)/sum(unit_count),
            sr_greater_than_4units = sum(sr_greater_than_4units)/sum(unit_count),
            cancellation_units = sum(cancellation_units)/sum(unit_count),
            cod_units = sum(cod_units)/sum(unit_count),
            fabreachunits = sum(fabreachunits)/sum(unit_count),
            nfabreachunits = sum(nfabreachunits)/sum(unit_count),
            promotional_discount = sum(promotional_discount)/sum(unit_count),
            ndd_sdd = sum(ndd_sdd)/sum(unit_count),
            avg_sla = mean(avg_sla),
            avg_o2d = mean(avg_o2d),
            avg_selling_price = mean(avg_selling_price),
            BGM_share = sum(BGM_share)/sum(unit_count),
            Electronics_share = sum(Electronics_share)/sum(unit_count),
            Furniture_share = sum(Furniture_share)/sum(unit_count),
            Home_share = sum(Home_share)/sum(unit_count),
            Large_share = sum(Large_share)/sum(unit_count),
            LifeStyle_share = sum(LifeStyle_share)/sum(unit_count),
            Mobile_share = sum(Mobile_share)/sum(unit_count),
            return_requests_items = sum(return_requests_items)/sum(unit_count),
            rvp_approval_count = sum(rvp_approval_count)/sum(unit_count),
            ss_approval_count = sum(ss_approval_count)/sum(unit_count),
            replacement_approval = sum(replacement_approval)/sum(unit_count),
            email_share = sum(email_share)/sum(responses),
            chat_share = sum(chat_share)/sum(responses),
            voice_share = sum(voice_share)/sum(responses),
            customer_cancellation_share = sum(customer_cancellation_qty)/sum(order_qty),
            fraud_tns_post_order_cancel_share = sum(fraud_tns_post_order_cancel_qty)/sum(order_qty),
            fk_group_cancel_share = sum(fk_group_cancel_qty)/sum(order_qty),
            frm_share = sum(frm_count)/sum(responses),
            e2e_share = sum(e2e_count)/sum(responses),
            customer_threads = sum(customer_threads)/sum(threads),
            note_threads = sum(note_threads)/sum(threads),
            rule_reponse_threads = sum(rule_reponse_threads)/sum(threads),
            staff_account_threads = sum(staff_account_threads)/sum(threads),
            rnr_threads = sum(rnr_threads)/sum(threads),
            pcd = mean(pcd, na.rm = T),
            resolution = mean(resolution, na.rm = T),
            soft_skills = mean(soft_skills, na.rm = T),
            communication_proficiency = mean(communication_proficiency, na.rm = T),
            compassion = mean(compassion, na.rm = T),
            composure = mean(composure, na.rm = T),
            customer_centricity = mean(customer_centricity, na.rm = T),
            agg_four_c = mean(agg_four_c, na.rm = T),
            tenurity = mean(tenurity, na.rm = T)
  )

max_week <- max(df_enterprise_model_post_ld_week$response_week)

df_enterprise_model_post_ld_week <- df_enterprise_model_post_ld_week[order(df_enterprise_model_post_ld_week$response_week),]

df_enterprise_model_post_ld_week <- df_enterprise_model_post_ld_week %>% 
  filter(response_week %in% seq((max_week-8), (max_week))) %>% 
  select(c(colnames(df_enterprise_model_post_ld)))

write.csv(df_enterprise_model_post_ld_week, 'enterprise_post_ld_weekwise_v4.csv')

