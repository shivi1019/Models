####  Feature selection for RR driver model ####

## Loading libraries ####

library(Boruta)# feature selection 
library(VGAM) 
library(pscl)
require(caTools)


## Setting working directory 
getwd()
setwd("~/Project1-WFM/Project3 - RR driver/")

## Loading dataset
# data_all <- read.csv("Processed data/Model_ADS_Boruta.csv", stringsAsFactors = F)

units(model_ads$diff_inc_solved_time_hrs) <- "hours" 
units(model_ads$order_del_first_last_time_hrs) = "hours" 
units(model_ads$return_completion_time_hrs) = "hours"  
units(model_ads$return_app_time_hrs) = "hours" 
units(model_ads$diff_dev_return_time_hrs) = "hours" 



model_ads_filt = model_ads %>% filter( diff_inc_solved_time_hrs >= 0,
                                      order_del_first_last_time_hrs >= 0,
                                      return_completion_time_hrs >= 0,
                                      return_app_time_hrs >= 0,
                                      diff_dev_return_time_hrs >= 0)




#splitting into train and test
sample = sample.split(model_ads_filt,SplitRatio = 0.80)#splits the data in the ratio mentioned in SplitRatio

train_data = subset( model_ads_filt, sample == T)

test = subset( model_ads_filt, sample == F)



## Missing 
length(unique(train_data$resolution_flag))


## replacing blanks with na 
train_data[train_data == ""] <- NA

train_data <- train_data[complete.cases(train_data),]

rownames(train_data) <- NULL



# =============== FEATURE SELECTION ================ #
## Using Boruta
set.seed(123)


# train_data$response <- ifelse(train_data$response == "yes", 1 , 0)
boruta_train <- Boruta(resolution_flag ~., data = train_data, doTrace = 1)

print(boruta_train)

## Plotting boruta ####
plot(boruta_train, xlab = "", xaxt = "n")

## Appending the feature vertically so that x label is visible ####
lz<-lapply(1:ncol(boruta_train$ImpHistory),function(i)
  boruta_train$ImpHistory[is.finite(boruta_train$ImpHistory[,i]),i])


names(lz) <- colnames(boruta_train$ImpHistory)

Labels <- sort(sapply(lz,median))

# Plotting Boruta with labels ####

axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta_train$ImpHistory), cex.axis = 0.7)

## Blue boxplots correspond to minimal, average and maximum Z score of a shadow attribute.
## Red, yellow and green boxplots represent Z scores of rejected, tentative and confirmed attributes respectively

## The tentative attributes will be classified as confirmed or rejected by comparing the median Z score of the attributes
## with the median Z score of the best shadow attribute

final_boruta <- TentativeRoughFix(boruta_train)
print(final_boruta)

col_list = as.list(colnames(final_boruta$ImpHistory))
class(col_list)

col_list[[68]] <- NULL
col_list[[69]] <- NULL
col_list[[70]] <- NULL


col_list = as.character(col_list)

# my_list = col_list[names(col_list) %in% c("shadowMax","shadowMin","shadowMean") == F] 

#####====================== Model creation classification  =======================#####

# model_1 <- glm(Survived ~.,family=binomial(link='logit'),data = data)
# summary(model_1)
# AIC = 336
## subsetting the data with only important features 

imp_feature_data = subset(train_data,select= col_list)

 model_2 <- glm(resolution_flag ~.,family=binomial(link='logit'),data = imp_feature_data)
summary(model_2)
#   AIC = 175.05 
## Model 2 is better as AIC is low 

final_model = glm(formula = target_response ~ ., family = binomial(link = "logit"), 
                  data = model_ads)

# Interpreting the results of our logistic regression model #
#### Anova test to analyze the table of deviance #### 

anova(model_ads, test="Chisq")

#### https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/


## McFadden R2 index can be used to assess the model fit ####

pR2(model_2)


## Assessing the predictive ability of the model ####

fitted_results <- predict(final_model,newdata=subset(test,select=c(1,4,5,9),type='response'))
fitted_results <- ifelse(fitted_results > 0.5,1,0)


length(fitted_results)

#misClasificError <- mean(fitted_results != train_data$Survived)

misClasificError <- fitted_results != train_data$Survived
table(misClasificError)["TRUE"]

diff = (118/418)*100

# Accuracy of the model #### 

print(paste('Accuracy',100-diff))


