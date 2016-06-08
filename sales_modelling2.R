# this file carries out sales modelling for (storeID, productID) tuples in the store_test data

# load packages
library(data.table)
library(h2o)

train <- readRDS("store_train.RData")
test <- readRDS("store_test.RData")

# remove columns not used in model
train[,c("total_quantity", "days_in_assort", "height", "expirationDays",
                "width", "depth", "availability", "total_shelf_space", 
                "mode_shelf_space"):= NULL]
test[,c("total_quantity", "days_in_assort", "height", "expirationDays",
         "width", "depth", "availability", "total_shelf_space", 
         "mode_shelf_space"):= NULL]

# columns to treat as factors
factor_vars <- c("productID", "storeID", "grup", "fam", "subFam", "sub_chain",
                 "chain", "town", "size", "community", "province", "units", 
                 "flavor", "type")

# update necessary columns to factors
train <- train[,(factor_vars):=lapply(.SD, as.factor),.SDcols=factor_vars, with=FALSE]
test <- test[,(factor_vars):=lapply(.SD, as.factor),.SDcols=factor_vars, with=FALSE]
rm(factor_vars)

# put response variable in first column
neworder <- c(names(train)[9], names(train)[-9])
setcolorder(train, neworder)
rm(neworder)

# set up data to use in predict function 
predict_dt <- as.data.table(as.data.frame(test))[,avg_sales_per_day:=NULL]



################################################################################
# Create Random Forest model 
################################################################################

# master_train must have the response variable in the first column
rf_model <- function(training_df, ntrees=50, nfolds=5, mtries=-1) {
    
    # install and load pacakges
    if (!require("h2o")) install.packages("h2o"); library(h2o)
    
    # initialise h2o
    h2o.init(nthreads = -1)
    
    # Map to h2o data frame
    train.h2o   <- as.h2o(training_df, destination_frame = "training")
    
    # run RF model
    model <- h2o.randomForest( x=2:ncol(training_df),
                               y=1, 
                               training_frame=train.h2o, 
                               nfolds=nfolds,
                               mtries=mtries,
                               ntrees=ntrees)
    return(model)
}



################################################################################
# Predict for existing (storeID, productID) tuples
################################################################################
# 'predict_dt' must be class data.table
predict_model <- function(predict_dt, model) {
    
    # install and load pacakges
    if (!require("h2o")) install.packages("h2o"); library(h2o)
    
    # initialise h2o
    h2o.init(nthreads = -1)
    
    # map to h2o object
    predict.h2o    <- as.h2o(predict_dt, destination_frame = "predict")
    
    # predictions
    preds <- h2o.predict(model, newdata = predict.h2o)
    preds <- as.data.frame(preds)
    
    # add storeID and productID to preds
    preds <- cbind(predict_dt[,.(storeID, productID)], preds)
    
    return(preds)
    
}



# run the model
rf_model_1 <- rf_model(train, nfolds=2)


# idea is to predict sales for the test data using the mkt share variables we already 
# have (ie predict for products that already exist in a store) and then
# artificially create the mkt share variable (since in real likfe we don't have them)
# We can then compare how well are model predicts for these scenarios.



################################################################################
# create artificially the mkt share variable for the test data
################################################################################

