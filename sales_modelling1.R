library(data.table)
library(biglm)
library(h2o)

setwd("~/Desktop/BGSE/Term3/MasterProject/GSE")

master_train <- readRDS("master_train.RData")
master_test <- readRDS("master_test.RData")


############################ Linear Regression #################################
train_y <- master_train$avg_sales_per_day
train_x <- master_train[,c("total_quantity", "days_in_assort", "height", "expirationDays",
                           "width", "depth", "availability", "total_shelf_space", 
                           "mode_shelf_space"):= NULL]
train_x <- train_x[,1:23, with=FALSE]
train <- cbind(train_y, train_x)
train <- as.data.frame(train)

test_x <- master_test[,c("total_quantity", "days_in_assort", "height", "expirationDays",
                         "width", "depth", "availability", "total_shelf_space", 
                         "mode_shelf_space"):= NULL]
test <- as.data.frame(test_x)


rm(master_train)
rm(master_test)

#model_lm <- lm(train_y ~ ., data=train)
preds <- predict(model_lm, data=test)

############################ RF - H2o #################################
####### Model
master_train <- readRDS("master_train.RData")

master_train[,c("total_quantity", "days_in_assort", "height", "expirationDays",
                "width", "depth", "availability", "total_shelf_space", 
                "mode_shelf_space"):= NULL]

# columns to treat as factors
factor_vars <- c("productID", "storeID", "grup", "fam", "subFam", "sub_chain",
                 "chain", "town", "size", "idComunidad", "idProvince", "units", 
                 "flavor", "type")

# update necessary columns to factors
master_train <- master_train[,(factor_vars):=lapply(.SD, as.factor),.SDcols=factor_vars, with=FALSE]

# initialise h2o
h2o.init(nthreads = -1)

# Map to H20 data frame
train.h2o   <- as.h2o(master_train, destination_frame = "Training")

train_names <- names(master_train)
rm(master_train)

# run RF model
model_rf_1 <- h2o.randomForest(x=train_names[-6],
                               y=train_names[6], 
                               training_frame=train.h2o, 
                               nfolds=5,
                               mtries=-1,
                               ntrees=50)
 
######### Predict
# read in test data
master_test <- readRDS("master_test.RData")

# update cols for test
master_test[,c("total_quantity", "days_in_assort", "height", "expirationDays",
               "width", "depth", "availability", "total_shelf_space", 
               "mode_shelf_space"):= NULL]

# columns to treat as factors
factor_vars <- c("productID", "storeID", "grup", "fam", "subFam", "sub_chain",
                 "chain", "town", "size", "idComunidad", "idProvince", "units", 
                 "flavor", "type")

# update necessary columns to factors
master_test <- master_test[,(factor_vars):=lapply(.SD, as.factor),.SDcols=factor_vars, with=FALSE]

# map to h2o object
test.h2o    <- as.h2o(master_test, destination_frame = "Test")

# make predictions
pred_rf_1 <- h2o.predict(model_rf_1, newdata = test.h2o)
pred_rf_1 <- as.data.frame(pred_rf_1)

#write file
write.csv(pred_rf_1, "rf_1_preds.csv")





h2o.shutdown()

