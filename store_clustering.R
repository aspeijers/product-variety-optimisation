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

saveRDS(stores_dummies, "stores_dummies_mktshareshelf.RData")

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

saveRDS(stores_dummies, "stores_dummies_mktsharesales.RData")

# separate storeID into a different vector
storeIDs <- stores_dummies$storeID
stores_dummies[,storeID:=NULL]

############################ cosine similarity #################################
#library(lsa)

c_lsa <- cosine(t(as.matrix(stores_dummies)))
# minimum similarity is 0.5941. Perhpas use mkt_share instead.


########################### k-means clustering #################################
# We DON'T  need to scale the variables because they are average to average 



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



################################################################################
                    # clusters visualisation #
################################################################################
#load packages
library(ggplot2)

# read in tables
cluster_store<- as.data.table(readRDS("storeID_cluster_mktshare_sales.RData"))
#change storeID as int type 
cluster_store$storeID = as.integer(as.character(cluster_store$storeID))
master_train <- readRDS("master_train_mktshare.RData")

# join
clusters <- merge(master_train, cluster_store, by="storeID", all.x=TRUE)
rm(master_train, cluster_store)

# change necessary columns to factors
factor_vars <- c("productID", "storeID", "grup", "fam", "subFam", "sub_chain",
                 "chain", "town", "size", "community", "province", "units", 
                 "flavor", "type", "fit.cluster")
clusters <- clusters[,(factor_vars):=lapply(.SD, as.factor), .SDcols=factor_vars, with=FALSE]


# plots - store vars - counts of stores
store_vars = c("sub_chain", "chain", "size", "town", "province", "community")

for (var in store_vars) {
    print(ggplot(clusters, aes_string("fit.cluster", fill=var)) + geom_bar())
}

# investigate sub_chains appearing in cluster 2
stores <- readRDS("stores.RData")
large_stores <- stores[size %in% c("Z1", "Z2")]
table(large_stores$sub_chain)
hist(large_stores$sub_chain)

clust2_subchains <- stores[sub_chain %in% c("3131", "3154")]
table(clust2_subchains$chain)

# plots - store vars - total_sales
for (var in store_vars) {
    print(ggplot(clusters, aes_string(x = "fit.cluster", y = "total_quantity", fill=var, group=var)) + 
        geom_bar(stat = "summary", fun.y=sum))
}


# plot by product group
ggplot(clusters, aes_string("fit.cluster", fill="grup")) + geom_bar()
ggplot(clusters, aes_string(x = "fit.cluster", y = "total_quantity", fill="grup", group="grup")) + 
    geom_bar(stat = "summary", fun.y=sum)

# plot by product type
ggplot(clusters, aes_string("fit.cluster", fill="type")) + geom_bar()
ggplot(clusters, aes_string(x = "fit.cluster", y = "total_quantity", fill="type", group="type")) + 
    geom_bar(stat = "summary", fun.y=sum)

# plot by product flavour
ggplot(clusters, aes_string("fit.cluster", fill="flavor")) + geom_bar()
ggplot(clusters, aes_string(x = "fit.cluster", y = "total_quantity", fill="flavor", group="flavor")) + 
    geom_bar(stat = "summary", fun.y=sum)

################################################################################
                        # different clusters #
################################################################################
stores_dummies <- readRDS("stores_dummies_mktsharesales.RData")

# separate storeID into a different vector
storeIDs <- stores_dummies$storeID
stores_dummies[,storeID:=NULL]

# k-means clusters
product_cols    <- names(stores_dummies)[3:109]
flavor_cols     <- names(stores_dummies)[110:119]
type_cols       <- names(stores_dummies)[120:122]
units_cols      <- names(stores_dummies)[123:128]
grup_cols       <- names(stores_dummies)[129:141]
fam_cols        <- names(stores_dummies)[142:182]
subfam_cols     <- names(stores_dummies)[183:243]
store_cols      <- names(stores_dummies)[244:1065]

# all vars
wss <- (nrow(stores_dummies)-1)*sum(apply(as.data.frame(stores_dummies),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(stores_dummies,centers=i, iter.max = 100)$withinss)
}
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# we choose 6 (for now)

# calc clusters
fit <- kmeans(stores_dummies, 6) 
data_cluster <- data.frame(storeIDs, fit$cluster)
names(data_cluster)[ncol(data_cluster)] <- "sales_store"

# all mkt shares, no stores vars
stores_dummies_subset <- stores_dummies[,c(product_cols, subfam_cols, fam_cols, grup_cols, flavor_cols, type_cols, units_cols), with=FALSE]

wss <- (nrow(stores_dummies_subset)-1)*sum(apply(as.data.frame(stores_dummies_subset),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(stores_dummies_subset,centers=i, iter.max = 100)$withinss)
}

plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# 6 clusters


# only product mktshare
stores_dummies_subset <- stores_dummies[,product_cols, with=FALSE]

wss <- (nrow(stores_dummies_subset)-1)*sum(apply(as.data.frame(stores_dummies_subset),2,var))

for (i in 2:50) {
    wss[i] <- sum(kmeans(stores_dummies_subset,centers=i)$withinss)
}

plot(1:50, wss[1:50], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# 15, uneven

# only subFam mktshare
stores_dummies_subset <- stores_dummies[,subfam_cols, with=FALSE]

wss <- (nrow(stores_dummies_subset)-1)*sum(apply(as.data.frame(stores_dummies_subset),2,var))

for (i in 2:50) {
    wss[i] <- sum(kmeans(stores_dummies_subset,centers=i)$withinss)
}

plot(1:50, wss[1:50], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# uneven

# only Fam mktshare
stores_dummies_subset <- stores_dummies[,fam_cols, with=FALSE]

wss <- (nrow(stores_dummies_subset)-1)*sum(apply(as.data.frame(stores_dummies_subset),2,var))

for (i in 2:50) {
    wss[i] <- sum(kmeans(stores_dummies_subset,centers=i)$withinss)
}

plot(1:50, wss[1:50], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


# only grup mktshare
stores_dummies_subset <- stores_dummies[,grup_cols, with=FALSE]

wss <- (nrow(stores_dummies_subset)-1)*sum(apply(as.data.frame(stores_dummies_subset),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(stores_dummies_subset,centers=i)$withinss)
}

plot(1:20, wss[1:20], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
#6


# only flavor mktshare
stores_dummies_subset <- stores_dummies[,flavor_cols, with=FALSE]

wss <- (nrow(stores_dummies_subset)-1)*sum(apply(as.data.frame(stores_dummies_subset),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(stores_dummies_subset,centers=i)$withinss)
}

plot(1:20, wss[1:20], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
#6

# only type mktshare
stores_dummies_subset <- stores_dummies[,type_cols, with=FALSE]

wss <- (nrow(stores_dummies_subset)-1)*sum(apply(as.data.frame(stores_dummies_subset),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(stores_dummies_subset,centers=i)$withinss)
}

plot(1:20, wss[1:20], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
#6


# subset, group, flavor and type
stores_dummies_subset <- stores_dummies[, c(grup_cols, flavor_cols, type_cols), with=FALSE]

wss <- (nrow(stores_dummies_subset)-1)*sum(apply(as.data.frame(stores_dummies_subset),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(stores_dummies_subset,centers=i, iter.max = 50)$withinss)
}

plot(1:20, wss[1:20], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# 10 clusters, not vary exponential


# calc clusters
fit <- kmeans(stores_dummies_subset, 10) 
data_cluster <- cbind(data_cluster, fit$cluster)
names(data_cluster)[ncol(data_cluster)] <- "sales_grupflavortype"


# only stores vars
stores_dummies_subset <- stores_dummies[,store_cols, with=FALSE]

wss <- (nrow(stores_dummies_subset)-1)*sum(apply(as.data.frame(stores_dummies_subset),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(stores_dummies_subset,centers=i, iter.max = 100)$withinss)
}

plot(1:20, wss[1:20], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
#very uneven



####### mkt share by shelf space dummies
stores_dummies <- readRDS("stores_dummies_mktshareshelf.RData")

# separate storeID into a different vector
storeIDs <- stores_dummies$storeID
stores_dummies[,storeID:=NULL]

# k-means clusters
product_cols    <- names(stores_dummies)[3:109]
flavor_cols     <- names(stores_dummies)[110:119]
type_cols       <- names(stores_dummies)[120:122]
units_cols      <- names(stores_dummies)[123:128]
grup_cols       <- names(stores_dummies)[129:141]
fam_cols        <- names(stores_dummies)[142:182]
subfam_cols     <- names(stores_dummies)[183:243]
store_cols      <- names(stores_dummies)[244:1065]


# all mkt shares (including store vars)
wss <- (nrow(stores_dummies)-1)*sum(apply(as.data.frame(stores_dummies),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(stores_dummies,centers=i, iter.max = 100)$withinss)
}

plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# 6

# calc clusters
fit <- kmeans(stores_dummies, 6) 
data_cluster <- cbind(data_cluster, fit$cluster)
names(data_cluster)[ncol(data_cluster)] <- "shelf_store"


# all mkt shares, no stores vars
stores_dummies_subset <- stores_dummies[,c(product_cols, subfam_cols, fam_cols, grup_cols, flavor_cols, type_cols, units_cols), with=FALSE]

wss <- (nrow(stores_dummies_subset)-1)*sum(apply(as.data.frame(stores_dummies_subset),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(stores_dummies_subset,centers=i, iter.max = 100)$withinss)
}

plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# uneven


# only product mktshare
stores_dummies_subset <- stores_dummies[,product_cols, with=FALSE]

wss <- (nrow(stores_dummies_subset)-1)*sum(apply(as.data.frame(stores_dummies_subset),2,var))

for (i in 2:50) {
    wss[i] <- sum(kmeans(stores_dummies_subset,centers=i, iter.max = 100)$withinss)
}

plot(1:50, wss[1:50], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# no exponential pattern

# only subFam mktshare
stores_dummies_subset <- stores_dummies[,subfam_cols, with=FALSE]

wss <- (nrow(stores_dummies_subset)-1)*sum(apply(as.data.frame(stores_dummies_subset),2,var))

for (i in 2:50) {
    wss[i] <- sum(kmeans(stores_dummies_subset,centers=i, iter.max = 100)$withinss)
}

plot(1:50, wss[1:50], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# no exponential pattern

# only Fam mktshare
stores_dummies_subset <- stores_dummies[,fam_cols, with=FALSE]

wss <- (nrow(stores_dummies_subset)-1)*sum(apply(as.data.frame(stores_dummies_subset),2,var))

for (i in 2:50) {
    wss[i] <- sum(kmeans(stores_dummies_subset,centers=i, iter.max = 100)$withinss)
}

plot(1:50, wss[1:50], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# not bad, not good


# only grup mktshare
stores_dummies_subset <- stores_dummies[,grup_cols, with=FALSE]

wss <- (nrow(stores_dummies_subset)-1)*sum(apply(as.data.frame(stores_dummies_subset),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(stores_dummies_subset,centers=i, iter.max = 100)$withinss)
}

plot(1:20, wss[1:20], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# ok


# only flavor mktshare
stores_dummies_subset <- stores_dummies[,flavor_cols, with=FALSE]

wss <- (nrow(stores_dummies_subset)-1)*sum(apply(as.data.frame(stores_dummies_subset),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(stores_dummies_subset,centers=i, iter.max = 100)$withinss)
}

plot(1:20, wss[1:20], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# ok

# only type mktshare
stores_dummies_subset <- stores_dummies[,type_cols, with=FALSE]

wss <- (nrow(stores_dummies_subset)-1)*sum(apply(as.data.frame(stores_dummies_subset),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(stores_dummies_subset,centers=i, iter.max = 100)$withinss)
}

plot(1:20, wss[1:20], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# a bit uneven


# subset, group, flavor and type
stores_dummies_subset <- stores_dummies[, c(grup_cols, flavor_cols, type_cols), with=FALSE]

wss <- (nrow(stores_dummies_subset)-1)*sum(apply(as.data.frame(stores_dummies_subset),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(stores_dummies_subset,centers=i, iter.max = 100)$withinss)
}

plot(1:20, wss[1:20], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# uneven

# subset, group, flavor
stores_dummies_subset <- stores_dummies[, c(grup_cols, flavor_cols), with=FALSE]
wss <- (nrow(stores_dummies_subset)-1)*sum(apply(as.data.frame(stores_dummies_subset),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(stores_dummies_subset,centers=i, iter.max = 100)$withinss)
}
plot(1:20, wss[1:20], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# ok, 10 clusters

# calc clusters
fit <- kmeans(stores_dummies_subset, 10, iter.max = 100) 
data_cluster <- cbind(data_cluster, fit$cluster)
names(data_cluster)[ncol(data_cluster)] <- "shelf_grupflavor"


################# mkt share sales and shelfspace
stores_dummies_sales <- readRDS("stores_dummies_mktsharesales.RData")
stores_dummies_shelf <- readRDS("stores_dummies_mktshareshelf.RData")

# separate storeID into a different vector
storeIDs <- stores_dummies$storeID
stores_dummies_sales[,storeID:=NULL]
stores_dummies_shelf[,storeID:=NULL]

# remove store vars from one tables
store_cols      <- names(stores_dummies_shelf)[244:1065]
stores_dummies_sales[,store_cols:=NULL, with=FALSE]

# combine tables
stores_dummies <- cbind(stores_dummies_sales, stores_dummies_shelf)

wss <- (nrow(stores_dummies)-1)*sum(apply(as.data.frame(stores_dummies),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(stores_dummies,centers=i, iter.max = 100)$withinss)
}

plot(1:20, wss[1:20], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# ok, 6 clusters

#now try without store vars
stores_dummies_subset <- stores_dummies[,store_cols:= NULL, with=FALSE]

wss <- (nrow(stores_dummies_subset)-1)*sum(apply(as.data.frame(stores_dummies_subset),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(stores_dummies_subset,centers=i, iter.max = 100)$withinss)
}

plot(1:20, wss[1:20], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# very similar to above. Deni prefers without. 6 clusters

# calc clusters
fit <- kmeans(stores_dummies_subset, 6, iter.max = 100) 
data_cluster <- cbind(data_cluster, fit$cluster)
names(data_cluster)[ncol(data_cluster)] <- "sales_shelf"

# renames storeid col and save
names(data_cluster)[1] <- "storeID"
saveRDS(data_cluster, "kmeans_clusters.RData")

rm(list=ls())


################################################################################
                    # spectral clustering #
################################################################################
library(kernlab)

stores_dummies_sales <- readRDS("stores_dummies_mktsharesales.RData")
stores_dummies_shelf <- readRDS("stores_dummies_mktshareshelf.RData")

# separate storeID into a different vector
storeIDs <- stores_dummies$storeID
stores_dummies_sales[,storeID:=NULL]
stores_dummies_shelf[,storeID:=NULL]

# remove store vars from one tables
store_cols      <- names(stores_dummies_shelf)[244:1065]
stores_dummies_sales[,store_cols:=NULL, with=FALSE]

# combine tables
stores_dummies <- cbind(stores_dummies_sales, stores_dummies_shelf)

sc <- specc(stores_dummies, centers=6)
plot(my.data, col=sc, pch=4)            # estimated classes (x)
points(my.data, col=obj$classes, pch=5)