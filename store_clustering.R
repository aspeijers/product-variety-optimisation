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
                                "mkt_Grup_store", "mkt_Type_store", "mkt_Flavor_store", "mkt_product_store_sales", 
                                "mkt_subFam_store_sales", "mkt_Fam_store_sales", 
                                "mkt_Grup_store_sales", "mkt_Flavor_store_sales", 
                                "mkt_Type_store_sales", "avg_sales_per_day",
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

############################ cosine similarity #################################
install.packages("lsa")
library(lsa)

c_lsa <- cosine(t(as.matrix(stores_dummies)))
# minimum similarity is 0.5941. Perhpas use mkt_share instead.


########################### k-means clustering #################################
# Determine number of clusters

# weighted sum of squares
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
centroids_kmeans <- aggregate(stores_dummies,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(stores_dummies, fit$cluster)

# append store ID
store_cluster <- cbind(storeIDs, mydata)
storeID_cluster <- store_cluster[,c("storeIDs", "fit.cluster")]
names(storeID_cluster)[1] <- "storeID"

saveRDS(storeID_cluster, "storeID_cluster_dummyapproach.RData")
saveRDS(centroids_kmeans, "centroids_dummyappproach.RData")

# clear environment
rm(list=ls())

################################################################################
# with mkt share for shelf space
################################################################################
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
master_train <- master_train[,c("mkt_product_store_sales", 
                                "mkt_subFam_store_sales", "mkt_Fam_store_sales", 
                                "mkt_Grup_store_sales", "mkt_Flavor_store_sales",
                                "mkt_Type_store_sales", "mkt_Units_store_sales",
                                "avg_sales_per_day","weights_kg" ):=NULL]

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

# multiply mkt_product_store by all the dummies relating to products: productID
mkt_share = master_train$mkt_product_store

for (i in seq(17,123)){
    print(i)
    product = master_train[,i, with = FALSE]
    master_train[,names(master_train)[i] := mkt_share*product]
}

# multiply mkt_product_store by all the dummies relating to products: subFam
mkt_share = master_train$mkt_subFam_store

for (i in seq(197,257)){
    print(i)
    subFam = master_train[,i, with = FALSE]
    master_train[,names(master_train)[i] := mkt_share*subFam]
}

# multiply mkt_product_store by all the dummies relating to products: fam
mkt_share = master_train$mkt_Fam_store

for (i in seq(156,196)){
    print(i)
    Fam = master_train[,i, with = FALSE]
    master_train[,names(master_train)[i] := mkt_share*Fam]
}

# multiply mkt_product_store by all the dummies relating to products: grup
mkt_share = master_train$mkt_Grup_store

for (i in seq(143,155)){
    print(i)
    grup = master_train[,i, with = FALSE]
    master_train[,names(master_train)[i] := mkt_share*grup]
}

# multiply mkt_product_store by all the dummies relating to products: flavor
mkt_share = master_train$mkt_Flavor_store

for (i in seq(124,133)){
    print(i)
    flavor = master_train[,i, with = FALSE]
    master_train[,names(master_train)[i] := mkt_share*flavor]
}

# multiply mkt_product_store by all the dummies relating to products: type
mkt_share = master_train$mkt_Type_store

for (i in seq(134,136)){
    print(i)
    type = master_train[,i, with = FALSE]
    master_train[,names(master_train)[i] := mkt_share*type]
}

# multiply mkt_product_store by all the dummies relating to products: units
mkt_share = master_train$mkt_Units_store

for (i in seq(137,142)){
    print(i)
    units = master_train[,i, with = FALSE]
    master_train[,names(master_train)[i] := mkt_share*units]
}

#remove the mkt_share variables
master_train[,c("mkt_product_store","mkt_subFam_store","mkt_Fam_store","mkt_Grup_store",        
                "mkt_Flavor_store","mkt_Type_store","mkt_Units_store"):=NULL]

# summing product mkt share over store
product_sums <- master_train[,1:116, with=FALSE]
cols_to_sum = c("total_quantity",names(product_sums)[9:116])

product_sums <- product_sums[, lapply(.SD, sum), by=.(storeID, sub_chain, 
                                                        chain, town, size, community, province),
                               .SDcols = cols_to_sum]


# taking unique mkt share for other product variables over store
product_vars_tounique <- master_train[,c(1,3:8,117:250), with=FALSE]
cols_to_unique <- names(product_vars_tounique)[8:141]

unique_element = function(vector){
    sort(unique(vector))[length(unique(vector))]
}

product_vars_tounique <- product_vars_tounique[, lapply(.SD, unique_element), by=.(storeID, sub_chain, chain, town, size, community, province),.SDcols = cols_to_unique]

# merge product mkt share and product mkt share cols
stores_dummies <- merge(product_sums, product_vars_tounique, by=c("storeID", "sub_chain", "chain", "town", "size", "community", "province"))
rm(product_sums, product_vars_tounique)


# add dummies for the store variables 
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

############################ cosine similarity #################################
#library(lsa)

c_lsa <- cosine(t(as.matrix(stores_dummies)))
# minimum similarity is 0.5941. Perhpas use mkt_share instead.


########################### k-means clustering #################################
# scale variables
## We need to scale it 
stores_dummies_scaled <- scale(stores_dummies)

# Determine number of clusters
wss <- (nrow(stores_dummies)-1)*sum(apply(as.data.frame(stores_dummies),2,var))

# check for 2 to 15 clusters
for (i in 2:15) {
    wss[i] <- sum(kmeans(stores_dummies,centers=i)$withinss)
}

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# we choose 6 (for now)


# K-Means Cluster Analysis
fit <- kmeans(stores_dummies_scaled, 6) 
# get cluster means 
aggregate(stores_dummies_scaled,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(stores_dummies_scaled, fit$cluster)

# append store ID
store_cluster <- cbind(storeIDs, mydata)
storeID_cluster <- store_cluster[,c("storeIDs", "fit.cluster")]
names(storeID_cluster)[1] <- "storeID"

saveRDS(storeID_cluster, "storeID_cluster_mktshare.RData")


################################################################################
# with mkt share for sales
################################################################################
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
                                "mkt_Grup_store", "mkt_Flavor_store",
                                "mkt_Type_store", "mkt_Units_store",
                                "avg_sales_per_day","weights_kg" ):=NULL]

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

# multiply mkt_product_store by all the dummies relating to products: productID
mkt_share = master_train$mkt_product_store_sales

for (i in seq(17,123)){
    print(i)
    product = master_train[,i, with = FALSE]
    master_train[,names(master_train)[i] := mkt_share*product]
}

# multiply mkt_product_store by all the dummies relating to products: subFam
mkt_share = master_train$mkt_subFam_store_sales

for (i in seq(197,257)){
    print(i)
    subFam = master_train[,i, with = FALSE]
    master_train[,names(master_train)[i] := mkt_share*subFam]
}

# multiply mkt_product_store by all the dummies relating to products: fam
mkt_share = master_train$mkt_Fam_store_sales

for (i in seq(156,196)){
    print(i)
    Fam = master_train[,i, with = FALSE]
    master_train[,names(master_train)[i] := mkt_share*Fam]
}

# multiply mkt_product_store by all the dummies relating to products: grup
mkt_share = master_train$mkt_Grup_store_sales

for (i in seq(143,155)){
    print(i)
    grup = master_train[,i, with = FALSE]
    master_train[,names(master_train)[i] := mkt_share*grup]
}

# multiply mkt_product_store by all the dummies relating to products: flavor
mkt_share = master_train$mkt_Flavor_store_sales

for (i in seq(124,133)){
    print(i)
    flavor = master_train[,i, with = FALSE]
    master_train[,names(master_train)[i] := mkt_share*flavor]
}

# multiply mkt_product_store by all the dummies relating to products: type
mkt_share = master_train$mkt_Type_store_sales

for (i in seq(134,136)){
    print(i)
    type = master_train[,i, with = FALSE]
    master_train[,names(master_train)[i] := mkt_share*type]
}

# multiply mkt_product_store by all the dummies relating to products: units
mkt_share = master_train$mkt_Units_store_sales

for (i in seq(137,142)){
    print(i)
    units = master_train[,i, with = FALSE]
    master_train[,names(master_train)[i] := mkt_share*units]
}

#remove the mkt_share variables
master_train[,c("mkt_product_store_sales","mkt_subFam_store_sales","mkt_Fam_store_sales","mkt_Grup_store_sales",        
                "mkt_Flavor_store_sales","mkt_Type_store_sales","mkt_Units_store_sales"):=NULL]

# summing product mkt share over store
product_sums <- master_train[,1:116, with=FALSE]
cols_to_sum = c("total_quantity",names(product_sums)[9:116])

product_sums <- product_sums[, lapply(.SD, sum), by=.(storeID, sub_chain, 
                                                      chain, town, size, community, province),
                             .SDcols = cols_to_sum]


# taking unique mkt share for other product variables over store
product_vars_tounique <- master_train[,c(1,3:8,117:250), with=FALSE]
cols_to_unique <- names(product_vars_tounique)[8:141]

unique_element = function(vector){
    sort(unique(vector))[length(unique(vector))]
}

product_vars_tounique <- product_vars_tounique[, lapply(.SD, unique_element), by=.(storeID, sub_chain, chain, town, size, community, province),.SDcols = cols_to_unique]

# merge product mkt share and product mkt share cols
stores_dummies <- merge(product_sums, product_vars_tounique, by=c("storeID", "sub_chain", "chain", "town", "size", "community", "province"))
rm(product_sums, product_vars_tounique)


# add dummies for the store variables 
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

############################ cosine similarity #################################
#library(lsa)

c_lsa <- cosine(t(as.matrix(stores_dummies)))
# minimum similarity is 0.5941. Perhpas use mkt_share instead.


########################### k-means clustering #################################
# We DON'T  need to scale the variables because they are average to average 

# Determine number of clusters
wss <- (nrow(stores_dummies)-1)*sum(apply(as.data.frame(stores_dummies),2,var))

# check for 2 to 15 clusters
for (i in 2:20) {
    wss[i] <- sum(kmeans(stores_dummies,centers=i)$withinss)
}

plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# we choose 6 (for now)


# K-Means Cluster Analysis
fit <- kmeans(stores_dummies, 6) 
# get cluster means 
centroids <- aggregate(stores_dummies,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(stores_dummies, fit$cluster)

# append store ID
store_cluster <- cbind(storeIDs, mydata)
storeID_cluster <- store_cluster[,c("storeIDs", "fit.cluster")]
names(storeID_cluster)[1] <- "storeID"

saveRDS(storeID_cluster, "storeID_cluster_mktshare_sales.RData")

#### Hirarchical 
### LOOK AT THE METHOD WARD?
# Ward Hierarchical Clustering
d <- dist(as.matrix(stores_dummies), method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2") 
plot(fit) # display dendogram
### Obviously 6 is good number of clusters 

groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")
