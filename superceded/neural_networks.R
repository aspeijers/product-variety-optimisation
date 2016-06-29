train = readRDS("store_train.RData")
library(dummies)
library(neuralnet)
train[,c("total_quantity", "days_in_assort", "height", "expirationDays",
         "width", "depth", "availability", "total_shelf_space", 
         "mode_shelf_space"):= NULL]
# test[,c("total_quantity", "days_in_assort", "height", "expirationDays",
#          "width", "depth", "availability", "total_shelf_space", 
#          "mode_shelf_space"):= NULL]

# columns to treat as factors
factor_vars <- c("productID", "storeID", "grup", "fam", "subFam", "sub_chain",
                 "chain", "town", "size", "community", "province", "units", 
                 "flavor", "type")

# update necessary columns to factors
train <- train[,(factor_vars):=lapply(.SD, as.factor),.SDcols=factor_vars, with=FALSE]

train[,town:=NULL]
# add dummies for the store variables 
dummy_vars <- c("storeID","productID","sub_chain", "chain", "size", "community", "province" )

# using Dummy pkg create dummy vars for each product
for (i in dummy_vars) {
  print(i)
  dummy_matrix <- dummy(i, train)
  dummy_matrix <- as.data.table(dummy_matrix)
  train <- cbind(dummy_matrix, train)
  train[,i:=NULL, with=FALSE]
}

rm(dummy_matrix, dummy_vars, i)

n <- names(train)
f <- as.formula(paste("avg_sales_per_day ~", paste(n[!n %in% "avg_sales_per_day"], collapse = " + ")))
nn <- neuralnet(f,data=train,hidden=c(5,3),linear.output=T)
                