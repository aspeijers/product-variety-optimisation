##################################################################################################
######## Description: Average the product market share sell-in over the clusters in the best clustering  
######## input: cluster_eval_mse.RData, clusterings_2.RData,master_mktshare.RData
######## output: predicted_mktshare.RData
##################################################################################################


library(data.table)

## load the tables for evaluation
clusters     <- readRDS("clusterings_2.RData")
cluster_eval <- readRDS("cluster_eval_mse.RData")
master       <- readRDS("master_mktshare.RData")

# check how many NA's are included in average for best clustering
#NAs <- cluster_eval$no_NAs[names(cluster_eval$no_NAs)==cluster_eval$best_clustering] #34

# decide that more than 10% of NA's is too much. 
ordered_avgs <- sort(cluster_eval$average_MSE, decreasing = FALSE)

# find best clustering fulfilling this constraint
for (i in 1:length(ordered_avgs)) {
    cl_name <- names(ordered_avgs)[i]
    NAs <- cluster_eval$no_NAs[names(cluster_eval$no_NAs)==cl_name]
    if (NAs <= 10) {
        best_cl_name <- cl_name
        cat("best clustering is: ", best_cl_name)
        break
    }
}


# subset the store clusters pertaining to the best clustering
best_cluster <- clusters[,c("storeID", best_cl_name) ]
names(best_cluster)[2] <- "cluster"

# merge store cluster number to master table
master_cl <- merge(master, best_cluster, by="storeID", all.x=TRUE)
master_cl <- as.data.table(master_cl)

# get all products
products <- unique(master_cl$productID)

# get all stores
stores <- unique(master_cl$storeID)

# create cartesian product table and merge cluster
predicted_mktshare <- CJ(stores, products)
names(predicted_mktshare) <- c("storeID", "productID")
predicted_mktshare <- merge(predicted_mktshare, best_cluster, by="storeID", all.x =TRUE)

average_mktshare   <- master_cl[,.(pred = mean(mkt_product_store_sales)), by=.(productID, cluster) ]
predicted_mktshare <- merge(predicted_mktshare, average_mktshare, by=c("productID", "cluster"), all.x=TRUE)

saveRDS(predicted_mktshare, "predicted_mktshare.RData")

