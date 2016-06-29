# this file carries out SVM sales modelling for (storeID, productID) tuples.
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

################################################################################
        		# model using all variables #
################################################################################
# First Pass SVM model - (2 folds)
control <- trainControl(method="cv", number=1)
seed <- 7
metric <- "RMSE" #root MSE
set.seed(seed)


registerDoMC(cores = 2)
svm_tune <- train(avg_sales_per_day~., 
                    data=train, 
                    method="svmRadial", 
                    metric=metric, 
                    tuneLength=1, 
                    trControl=control,
                    allowParallel = F
                    #preProcess = c("center", "scale")
                    )

####
# Second Pass SVM model - (2 folds)
control <- trainControl(method="cv", number=2)
seed <- 7
metric <- "RMSE"
tunegrid <- expand.grid(sigma = c(.01, .015, 0.2), 
						C = c(0.75, 0.9, 1, 1.1, 1.25))
set.seed(seed)


registerDoMC(cores = 2)
svm_tune2 <- train(avg_sales_per_day~., 
                    data=train, 
                    method="svmRadial", 
                    metric=metric, 
                    tuneGrid=tunegrid, 
                    trControl=control, 
                    #preProcess = c("center", "scale"),
                    importance=FALSE)




