library(data.table)
library(reshape)
library(ggplot2)

# read in necessary tables
master_train <- readRDS("master_train_mktshare.RData")
master_test <- readRDS("master_test_mktshare.RData")
data_clusters <- readRDS("kmeans_clusters.RData")

# create test and train for each of the 5 clusterings in data_clusters
test <- master_test[,.(storeID, productID, avg_sales_per_day)]
train <- master_train[,.(storeID, productID, avg_sales_per_day)]
data_clusters$storeID <- as.integer(as.character(data_clusters$storeID))
rm(master_train, master_test)

# merge the cluster number to test
test1 <- merge(test, data_clusters[,1:2], by="storeID", all.x=TRUE)
test2 <- merge(test, data_clusters[,c(1,3)], by="storeID", all.x=TRUE)
test3 <- merge(test, data_clusters[,c(1,4)], by="storeID", all.x=TRUE)
test4 <- merge(test, data_clusters[,c(1,5)], by="storeID", all.x=TRUE)
test5 <- merge(test, data_clusters[,c(1,6)], by="storeID", all.x=TRUE)
names(test1)[4] <- "cluster"
names(test2)[4] <- "cluster"
names(test3)[4] <- "cluster"
names(test4)[4] <- "cluster"
names(test5)[4] <- "cluster"
# remove observations for new stores (we might deal with them later)
test1 <- test1[!is.na(cluster)]
test2 <- test2[!is.na(cluster)]
test3 <- test3[!is.na(cluster)]
test4 <- test4[!is.na(cluster)]
test5 <- test5[!is.na(cluster)]

# merge the cluster number to train
train1 <- merge(train, data_clusters[,1:2], by="storeID", all.x=TRUE)
train2 <- merge(train, data_clusters[,c(1,3)], by="storeID", all.x=TRUE)
train3 <- merge(train, data_clusters[,c(1,4)], by="storeID", all.x=TRUE)
train4 <- merge(train, data_clusters[,c(1,5)], by="storeID", all.x=TRUE)
train5 <- merge(train, data_clusters[,c(1,6)], by="storeID", all.x=TRUE)
names(train1)[4] <- "cluster"
names(train2)[4] <- "cluster"
names(train3)[4] <- "cluster"
names(train4)[4] <- "cluster"
names(train5)[4] <- "cluster"

rm(train, test)


# function to calculate errors given by using a certain clustering to predict sales in the test data
# input:    the test and train datafames:  columns are storeID, productID, avg_sales and cluster 
# output:   a list containing residuals, MSE and MAE
cluster_evaluation <- function( test, train ) {
    
    # average the sales in train when grouped by cluster and product
    train_agg <- train[,.(mean_sales = mean(avg_sales_per_day)),by=.(cluster,productID)]
    setkey(train_agg,cluster, productID)
    
    # split training data into one data frame for each cluster. Creates a list of data.frames
#     no_clusters <- max(train$cluster)
#     names <- paste0("cluster", 1:no_clusters)
#     new_df <- sapply(names, function(x) {data.table()})
#     for (cl in 1:no_clusters) {
#         new_df[[cl]] <- train[cluster==cl]
#     }

    # predict sales
    pred_sales <- c()
    for (i in 1:nrow(test)) {
        clust <- as.integer(as.data.frame(test[i,4, with=FALSE]))
        prod <- as.character(as.data.frame(test[i,2, with=FALSE]))
        pred_sales <- c(pred_sales, train_agg[cluster==clust & productID == prod,]$mean_sales)
    }
    
    #calculate mean square error
    pred_errors <- test$avg_sales_per_day - pred_sales
    MSE <- sum(pred_errors^2)/length(pred_errors)
    MAE <- sum(abs(pred_errors))/length(pred_errors)
    return(list(pred_errors = pred_errors, MSE=MSE, MAE=MAE))
}



# run the function for each of the 5 clusterings and Save
clustering1_pred <- cluster_evaluation(test1, train1)
clustering2_pred <- cluster_evaluation(test2, train2)
clustering3_pred <- cluster_evaluation(test3, train3)
clustering4_pred <- cluster_evaluation(test4, train4)
clustering5_pred <- cluster_evaluation(test5, train5)
saveRDS(clustering1_pred, "clustering1_pred.RData")
saveRDS(clustering2_pred, "clustering2_pred.RData")
saveRDS(clustering3_pred, "clustering3_pred.RData")
saveRDS(clustering4_pred, "clustering4_pred.RData")
saveRDS(clustering5_pred, "clustering5_pred.RData")

# take out MSE and MAE from each clustering to compare
clusterMSE <- c(clustering1_pred$MSE, clustering2_pred$MSE, clustering3_pred$MSE, clustering4_pred$MSE, clustering5_pred$MSE )
clusterMAE <- c(clustering1_pred$MAE, clustering2_pred$MAE, clustering3_pred$MAE, clustering4_pred$MAE, clustering5_pred$MAE )
cluster_residuals <- as.data.frame(cbind(clustering1_pred$pred_errors, clustering2_pred$pred_errors, clustering3_pred$pred_errors, clustering4_pred$pred_errors, clustering5_pred$pred_errors))
names(cluster_residuals) <- c("cl1", "cl2", "cl3", "cl4", "cl5" )
cluster_residuals <- melt(cluster_residuals)


# plot the residuals for each of the 5 clusterings and compare
ggplot(cluster_residuals, aes(factor(variable), value)) + 
    coord_cartesian(ylim = c(-25,25)) +
    geom_boxplot()
    
# compare MSE and MAE
clusterMSE
clusterMAE

# clusterin 4 has the lowest MSE but the highest MAE. 
# Clusterings 1,3 and 5 have the next lowest MSEs. Out of these clustering 5 has the lowest MSE. 

# Clustering 5: corresponds to clustering based on all the mkt share by sales and mkt share by shelf 
# space variables, but without the store variables. Under this clustering there are 6 clusters.


####################################################################################
# we now compare the clusterings with simply averaging over all stores (the basic model)
test_noclust <- cbind(test1[,.(storeID, productID, avg_sales_per_day)], cluster=rep(1,nrow(test1)))
train_noclust <- cbind(train1[,.(storeID, productID, avg_sales_per_day)], cluster=rep(1,nrow(train1)))
clustering_noclust_pred <- cluster_evaluation(test_noclust, train_noclust)

# results are similar to clusterings 