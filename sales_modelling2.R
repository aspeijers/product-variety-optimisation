# this file carries out sales modelling for (storeID, productID) tuples.
# data has been split randomly and test data has (some) mkt share variables calculated from clusters
# the model is trained and parameters tuned using the training data and CV.
# we then use the model to predict sales for productID's that do not currently exist in stores.


# load packages
library(data.table)
library(caret)
library(randomForest)
library(doMC)

# load training data and validation test data 
# the test dta
train <- readRDS("store_train.RData")
# test <- readRDS("store_test.RData")

# remove columns not used in model
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
# test <- test[,(factor_vars):=lapply(.SD, as.factor),.SDcols=factor_vars, with=FALSE]
rm(factor_vars)

# put response variable in first column
neworder <- c(names(train)[-9], names(train)[9])
setcolorder(train, neworder)
rm(neworder)

# set up data to use in predict function 
# predict_dt <- as.data.table(as.data.frame(test))[,avg_sales_per_day:=NULL]



################################################################################
        # model using all variables, then do variable selection #
################################################################################
# Base line RF model - default parameters (2 folds)
control <- trainControl(method="cv", number=2, verboseIter = TRUE)
seed <- 7
metric <- "RMSE"
set.seed(seed)
mtry <- sqrt(ncol(train)-1)
tunegrid <- expand.grid(.mtry=mtry)

registerDoMC(cores = 2)
rf_default <- train(avg_sales_per_day~., 
                    data=train, 
                    method="rf", 
                    metric=metric, 
                    tuneGrid=tunegrid, 
                    trControl=control, 
                    importance=TRUE) #approx 3hrs
print(rf_default)
#varImp(rf_default) #need to set importance=TRUE in train() to be able to get this
#varImp(rf_default$finalModel)
rf_default$finalModel
rf_default$results
rf_default$times

# save workspace
save.image("~/Desktop/BGSE/Term3/MasterProject/GSE/rf_default.RData")
rm(rf_default)


################################################################################
# Base line RF model - default parameters (2 folds, variable importance returned)
control <- trainControl(method="cv", number=2, verboseIter = TRUE)
seed <- 7
metric <- "RMSE"
set.seed(seed)
mtry <- sqrt(ncol(train)-1)
tunegrid <- expand.grid(.mtry=mtry)

registerDoMC(cores = 2)
rf_default <- train(avg_sales_per_day~., 
                    data=train, 
                    method="rf", 
                    metric=metric, 
                    tuneGrid=tunegrid, 
                    trControl=control, 
                    importance=TRUE) # approx 33 hrs
print(rf_default)
varImp(rf_default)
varImp(rf_default$finalModel)
rf_default$finalModel
rf_default$results
rf_default$times

# save workspace
save.image("~/Desktop/BGSE/Term3/MasterProject/GSE/rf_default_varImps.RData")
rm(rf_default)


################################################################################
# Base line RF model - random search for parameters (2 folds -  no parallelization)
control <- trainControl(method="cv", number=2, verboseIter = TRUE, search="random")
seed <- 7
metric <- "RMSE"
set.seed(seed)
mtry <- sqrt(ncol(train)-1)

detach("package:doMC", unload=TRUE)
#registerDoMC(cores = 2)
rf_random <- train(avg_sales_per_day~., 
                     data=train, 
                     method="rf", 
                     metric=metric, 
                     tuneLength=15, 
                     trControl=control, 
                     importance=FALSE)
print(rf_random)
plot(rf_random)
#varImp(rf_random) 
#varImp(rf_default$finalModel)
rf_random$finalModel
rf_random$results
rf_random$times

# save workspace
save.image("~/Desktop/BGSE/Term3/MasterProject/GSE/rf_random.RData")
rm(rf_random)