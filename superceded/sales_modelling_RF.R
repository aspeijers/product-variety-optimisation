# This script contains 3 funtions:
# 1) Runs a random forest model (h2o) on the master_train data (only alltimers).
# 2) Makes predictions for (productID, storeID) tuples that exist in master_train.
# 3) For a given store, predicts for productID's that don't exist in the master_train
# by averaging over results in stores in the same cluster.





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


################################################################################
# Predict for non-existing (storeID, productID) tuples for a particular store
################################################################################
# takes as input a store ID, the output of the predict_model function (data.table),
# all alltimer products (vector) and the store clusters (data.frame)
predict_sales <- function( storeID, model_preds, all_products, clusters ) {
    
    store <- storeID
    
    # take model predictions relating to the store of interest
    preds_store     <- model_preds[ model_preds$storeID==store, ]
    preds_store[,storeID:=NULL]
    existing_prods  <- as.character(preds_store$productID)
    
    #f ind the products that do not currently exist
    missing_prods   <- setdiff(all_products, existing_prods)
    
    # find store cluster number
    store_clust     <- clusters[clusters$storeID==store, ]$fit.cluster
    
    # subset storeID's that are in the same cluster
    similar_stores  <- as.character( clusters[clusters$fit.cluster==store_clust, ]$storeID )
    
    # find sales predictions for stores in the same cluster
    same_clust_preds <- model_preds[model_preds$storeID %in% similar_stores & model_preds$productID %in% missing_prods,]
    
    # for new products, average sales over all other stores in the same cluster (where they exist)
    new_preds <- as.data.frame(matrix(NA, length(missing_prods), 2))
    for (i in 1:length(missing_prods)) {
        prod    <- missing_prods[i]
        preds   <- same_clust_preds[same_clust_preds$productID==prod, ]$predict
        predict <- mean(preds)
        new_preds[i,1] <- prod
        new_preds[i,2] <- predict
    }
    
    names(new_preds) <- c("productID", "predict")
    
    # combine with insample predictions
    preds_store_full    <- rbind(preds_store, new_preds)
    
    return(preds_store_full)    
}



############################ Using the Functions ###############################
# read in the master_train table
master_train <- readRDS("master_train_mktshare.RData")

# remove columns not used in model
master_train[,c("total_quantity", "days_in_assort", "height", "expirationDays",
                "width", "depth", "availability", "total_shelf_space", 
                "mode_shelf_space"):= NULL]

# columns to treat as factors
factor_vars <- c("productID", "storeID", "grup", "fam", "subFam", "sub_chain",
                 "chain", "town", "size", "community", "province", "units", 
                 "flavor", "type")

# update necessary columns to factors
master_train <- master_train[,(factor_vars):=lapply(.SD, as.factor),.SDcols=factor_vars, with=FALSE]

# put response variable in first column
neworder <- c(names(master_train)[9], names(master_train)[-9])
setcolorder(master_train, neworder)

# set up data to use in predict function 
predict_dt <- as.data.table(as.data.frame(master_train))[,avg_sales_per_day:=NULL]

# get all alltimer products
products <- readRDS("products.RData")
all_products <- products[availability=="alltimer", productID]
rm(products)

# import store clustering
clusters <- readRDS("storeID_cluster_mktshare_sales.RData")



# create model
rf_model_1 <- rf_model(master_train, nfolds=2)

# predict for existing (storeID, productID) tuples
preds_rf_model_1 <- predict_model(predict_dt, rf_model_1)

# shut down h2o instance
h2o.shutdown()

# predict for non-existing (storeID, productID) tuples
preds_rf_model_1_all <- predict_sales(1003, preds_rf_model_1, all_products, clusters)

saveRDS(preds_rf_model_1_all, "preds_rf_model_1.RData")


