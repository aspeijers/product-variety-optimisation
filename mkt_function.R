# average the product market share (by sales) over clusters from the best clustering

library(data.table)


## load the tables for evaluation
clusters = readRDS("cluster.RData")
cluster_eval = readRDS("cluster_eval_mse.RData")
master <- readRDS("master_mktshare.RData")

best_cluster <- clusters[,c("storeID", paste(cluster_eval[[1]])) ]
names(best_cluster)[2] <- "cluster"
best_cluster$storeID <- as.integer(as.character(best_cluster$storeID))

# count how many products are covered in each cluster
no_clusters <- max(best_cluster$cluster)

master_cl <- merge(master, best_cluster, by="storeID", all.x=TRUE)
master_cl <- as.data.table(master_cl)

no_products <- master_cl[,.(no_prods = length(unique(productID))), by=cluster] # every cluster has 107 (all alltimer) products

# get all products
products <- unique(master_cl$productID)

# get all stores
stores <- unique(master_cl$storeID)

# create cartesian product table and merge cluster
predicted_mktshare <- CJ(stores, products)
names(predicted_mktshare) <- c("storeID", "productID")
predicted_mktshare <- merge(predicted_mktshare, best_cluster, by="storeID", all.x =TRUE)

average_mktshare <- master_cl[,.(pred = mean(mkt_product_store_sales)), by=.(productID, cluster) ]
predicted_mktshare <- merge(predicted_mktshare, average_mktshare, by=c("productID", "cluster"), all.x=TRUE)

saveRDS(predicted_mktshare, "predicted_mktshare.RData")

