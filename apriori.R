## Funciton for apriori

## Loading libraries 

library(arules)

## defining function 

apriori_itemset = function(trans_df, tran_id_col, product_name_col, minsup, confidence){
  
  demo_data = trans_df
  
  
  ## coverting the data set into transaction data 
  
  trans_data <- as(split(demo_data[,tran_id_col], demo_data[,product_name_col]), "transactions")
  
  ## Viewing the results of transaction data
  
  inspect(trans_data)
  
  ## Mine association rules.
  
  rules <- apriori(trans_data,parameter = list(supp = minsup, conf = confidence, target = "rules"))
  summary(rules)
  
  
  ## viewing rules 
  
  l = length(rules)
  
  everything = labels(rules)
  print("============Viewing rules===============")
  print(everything)
  
  ## Viewing only the output vector 
  print("============Viewing only the output vector===============")
  
  cut = unlist(strsplit(everything,"=> "))[seq(2,2*l,by=2)]
  print(cut)
  
}

## Calling the funciton 

apriori_itemset(demo_data,"department" , "order_id", 0.1 , 0.9)


