library(data.table)
library(h2o)

setwd("~/Desktop/BGSE/Term3/MasterProject/GSE")


############################ Linear Regression #################################
master_train <- readRDS("master_train_mktshare.RData")
master_test <- readRDS("master_test_mktshare.RData")

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
predict.rf1 <- function(storeID_current) {
    
    library(h2o)
    
    ####### Model
    master_train <- readRDS("master_train_mktshare.RData")
    
    master_train[,c("total_quantity", "days_in_assort", "height", "expirationDays",
                    "width", "depth", "availability", "total_shelf_space", 
                    "mode_shelf_space"):= NULL]
    
    # columns to treat as factors
    factor_vars <- c("productID", "storeID", "grup", "fam", "subFam", "sub_chain",
                     "chain", "town", "size", "community", "province", "units", 
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
    model_rf_1 <- h2o.randomForest(x=train_names[-9],
                                   y=train_names[9], 
                                   training_frame=train.h2o, 
                                   nfolds=2,
                                   mtries=-1,
                                   ntrees=50)
    
    ######### Predict Function
    
    
    # read in test data
    master_test <- readRDS("master_test_mktshare.RData")
    
    # update cols for test
    master_test[,c("total_quantity", "days_in_assort", "height", "expirationDays",
                   "width", "depth", "availability", "total_shelf_space", 
                   "mode_shelf_space"):= NULL]
    
    # columns to treat as factors
    factor_vars <- c("productID", "storeID", "grup", "fam", "subFam", "sub_chain",
                     "chain", "town", "size", "community", "province", "units", 
                     "flavor", "type")
    
    # update necessary columns to factors
    master_test <- master_test[,(factor_vars):=lapply(.SD, as.factor),.SDcols=factor_vars, with=FALSE]
    
    # map to h2o object
    test.h2o    <- as.h2o(master_test, destination_frame = "Test")
    
    # make predictions
    pred_rf_1 <- h2o.predict(model_rf_1, newdata = test.h2o)
    pred_rf_1 <- as.data.frame(pred_rf_1)
    
    pred_rf_1 <- cbind(master_test[,.(storeID, productID)], pred_rf_1)
    
    rm(master_test)
    #h2o.shutdown()
    #Y
    
    #write file
    #write.csv(pred_rf_1, "rf_1_preds.csv")
    
    # Now predict for products that are not present in stores
    #storeID_current <- 1003
    preds_store <- pred_rf_1[pred_rf_1$storeID==storeID_current, ]
    preds_store[,storeID:=NULL]
    current_prods <- as.character(preds_store$productID)
    
    #find the products that do not exist
    products <- readRDS("products.RData")
    all_prods <- products[availability=="alltimer", productID]
    missing_prods <- setdiff(all_prods, current_prods)
    
    # import store clustering
    storeID_cluster <- readRDS("storeID_cluster_mktshare_sales.RData")
    current_store_clust <- storeID_cluster[storeID_cluster$storeID==storeID_current, ]$fit.cluster
    
    # subset storeID's that are in the same cluster
    same_clust_storeIDs <- as.character(storeID_cluster[storeID_cluster$fit.cluster==current_store_clust, ]$storeID)
    
    # find predictions of similar stores
    same_clust_preds <- pred_rf_1[pred_rf_1$storeID %in% same_clust_storeIDs & pred_rf_1$productID %in% missing_prods,]
    
    oos_preds <- as.data.frame(matrix(NA, length(missing_prods), 2))
    for (i in 1:length(missing_prods)) {
        prod <- missing_prods[i]
        preds <- same_clust_preds[same_clust_preds$productID==prod, ]$predict
        predict <- mean(preds)
        oos_preds[i,1] <- prod
        oos_preds[i,2] <- predict
    }
    
    names(oos_preds) <- c("productID", "predict")
    
    # combine with insample predictions
    preds_store_full <- rbind(preds_store, oos_preds)
    
    return(preds_store_full)
    
    
}

