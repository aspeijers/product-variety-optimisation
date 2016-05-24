library(data.table)
library(dummies)

master_train <- readRDS("master_train_mktshare.RData")

# remove columns that will not be used in models
master_train[,c("days_in_assort", "height", "expirationDays",
                "width", "depth", "availability", 
                "mode_shelf_space"):= NULL]

# columns to treat as factors
factor_vars <- c("productID", "storeID", "grup", "fam", "subFam", "sub_chain",
                 "chain", "town", "size", "community", "province", "units", 
                 "flavor", "type")

# update necessary columns to factors
master_train <- master_train[,(factor_vars):=lapply(.SD, as.factor),.SDcols=factor_vars, with=FALSE]


# remove unnecessary cols
master_train <- master_train[,c("mkt_product_store", 
                                "mkt_subFam_store", "mkt_Fam_store", 
                                "mkt_Grup_store", "mkt_product_store_sales", 
                                "mkt_subFam_store_sales", "mkt_Fam_store_sales", 
                                "mkt_Grup_store_sales","avg_sales_per_day",
                                "weights_kg" ):=NULL]

# variables for which we want to create dummy columns
dummy_vars <- c("productID", "flavor", "type", "units", "grup", "fam", "subFam" )

# using Dummy pkg create dummy vars for each product
for (i in dummy_vars) {
    print(i)
    dummy_matrix <- dummy(i, master_train)
    dummy_matrix <- as.data.table(dummy_matrix)
    master_train <- cbind(master_train, dummy_matrix)
    master_train[,i:=NULL, with=FALSE]
}

rm(dummy_matrix)

# sum for each store
cols_to_sum <- c(names(master_train)[9:ncol(master_train)], "total_quantity")
stores_dummies <- master_train[, lapply(.SD, sum), by=.(storeID, sub_chain, chain, town, size, community, province), .SDcols = cols_to_sum]

# create dummies for store vars
dummy_vars <- c("sub_chain", "chain", "town", "size", "community", "province" )

# using Dummy pkg create dummy vars for each product
for (i in dummy_vars) {
    print(i)
    dummy_matrix <- dummy(i, stores_dummies)
    dummy_matrix <- as.data.table(dummy_matrix)
    stores_dummies <- cbind(stores_dummies, dummy_matrix)
    stores_dummies[,i:=NULL, with=FALSE]
}

rm(dummy_matrix)

# separate storeID into a different vector
storeIDs <- stores_dummies$storeID
stores_dummies[,storeID:=NULL]

# cosine similarity
cos.sim <- function(ix) {
    print(ix)
    A = stores_dummies[ix[1],]
    B = stores_dummies[ix[2],]
    return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
} 

n <- nrow(stores_dummies) 
cmb <- expand.grid(i=1:n, j=1:n) 
C <- matrix(apply(cmb,1,cos.sim),n,n)

# cosine similarity 2
install.packages("lsa")
library(lsa)

c_lsa <- cosine(t(as.matrix(stores_dummies)))
# minimum similarity is 0.5941. Perhpas use mkt_share instead.


########################### k-means clustering #################################
# Determine number of clusters
wss <- (nrow(stores_dummies)-1)*sum(apply(as.data.frame(stores_dummies),2,var))

for (i in 2:15) {
    wss[i] <- sum(kmeans(stores_dummies,centers=i)$withinss)
}

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# we choose 6 (for now)


# K-Means Cluster Analysis
fit <- kmeans(stores_dummies, 6) 
# get cluster means 
aggregate(stores_dummies,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(stores_dummies, fit$cluster)

# append store ID
store_cluster <- cbind(storeIDs, mydata)
storeID_cluster <- store_cluster[,c("storeIDs", "fit.cluster")]
names(storeID_cluster)[1] <- "storeID"

saveRDS(storeID_cluster, "storeID_cluster.RData")
