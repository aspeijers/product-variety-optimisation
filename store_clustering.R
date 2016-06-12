# do 5 different types of clustering on the entire master table and then evaluate clusters using CV

# load packages
library(data.table)
library(dummies)

# read in master
master <- readRDS("master_mktshare.RData")

# remove columns that will not be used in models
master[,c("days_in_assort", "expirationDays", "height", "width", "depth", 
          "availability", "mode_shelf_space"):= NULL]

# columns to treat as factors
factor_vars <- c("productID", "storeID", "grup", "fam", "subFam", "sub_chain",
                 "chain", "town", "size", "community", "province", "units", 
                 "flavor", "type")

# update necessary columns to factors
master <- master[,(factor_vars):=lapply(.SD, as.factor),.SDcols=factor_vars, with=FALSE]
rm(factor_vars)

################################################################################
# create mkt share by shelf space dummies #
################################################################################
# make copy of master for calculating mkt share by shelf space dummies
master_shelf <- as.data.table(as.data.frame(master))

# remove unnecessary cols
master_shelf <- master_shelf[,c("mkt_product_store_sales", "mkt_subFam_store_sales", 
                    "mkt_Fam_store_sales", "mkt_Grup_store_sales", 
                    "mkt_Flavor_store_sales", "mkt_Type_store_sales", 
                    "mkt_Units_store_sales", "avg_sales_per_day","weights_kg", 
                    "total_quantity"):=NULL]

# variables for which we want to create dummy columns
dummy_vars <- c("productID", "subFam", "fam", "grup", "flavor", "type", "units" )

# using Dummy pkg create dummy vars for each product
for (i in dummy_vars) {
    print(i)
    dummy_matrix <- dummy(i, master_shelf)
    dummy_matrix <- as.data.table(dummy_matrix)
    master_shelf <- cbind(master_shelf, dummy_matrix)
    master_shelf[,i:=NULL, with=FALSE]
}
rm(dummy_matrix, dummy_vars, i)

# multiply mkt_product_store by all the dummies relating to products: productID
mkt_share <- master_shelf$mkt_product_store

for (i in seq(17,123)){
    print(i)
    product = master_shelf[,i, with = FALSE]
    master_shelf[,names(master_shelf)[i] := mkt_share*product]
}

# multiply mkt_product_store by all the dummies relating to products: subFam
mkt_share = master_shelf$mkt_subFam_store

for (i in seq(124,184)){
    print(i)
    subFam = master_shelf[,i, with = FALSE]
    master_shelf[,names(master_shelf)[i] := mkt_share*subFam]
}

# multiply mkt_product_store by all the dummies relating to products: fam
mkt_share = master_shelf$mkt_Fam_store

for (i in seq(185,225)){
    print(i)
    Fam = master_shelf[,i, with = FALSE]
    master_shelf[,names(master_shelf)[i] := mkt_share*Fam]
}

# multiply mkt_product_store by all the dummies relating to products: grup
mkt_share = master_shelf$mkt_Grup_store

for (i in seq(226,238)){
    print(i)
    grup = master_shelf[,i, with = FALSE]
    master_shelf[,names(master_shelf)[i] := mkt_share*grup]
}

# multiply mkt_product_store by all the dummies relating to products: flavor
mkt_share = master_shelf$mkt_Flavor_store

for (i in seq(239,248)){
    print(i)
    flavor = master_shelf[,i, with = FALSE]
    master_shelf[,names(master_shelf)[i] := mkt_share*flavor]
}

# multiply mkt_product_store by all the dummies relating to products: type
mkt_share = master_shelf$mkt_Type_store

for (i in seq(249,251)){
    print(i)
    type = master_shelf[,i, with = FALSE]
    master_shelf[,names(master_shelf)[i] := mkt_share*type]
}

# multiply mkt_product_store by all the dummies relating to products: units
mkt_share = master_shelf$mkt_Units_store

for (i in seq(252,257)){
    print(i)
    units = master_shelf[,i, with = FALSE]
    master_shelf[,names(master_shelf)[i] := mkt_share*units]
}

rm(product, subFam, Fam, grup, flavor, type, units, i, mkt_share)


#remove the original mkt_share variables
master_shelf[,c("mkt_product_store","mkt_subFam_store","mkt_Fam_store", "mkt_Grup_store", 
                "mkt_Flavor_store","mkt_Type_store","mkt_Units_store"):=NULL]

# summing product mkt share and product total shelf space over store.
# NB. we could also take the unique value that isn't zero (if it exists, otherwise zero). 
product_sums <- master_shelf[,1:116, with=FALSE]
cols_to_sum <- c("total_shelf_space", names(product_sums)[10:116])
product_sums <- product_sums[,lapply(.SD, sum), 
                             by=.(storeID, sub_chain, chain, town, size, community, province), 
                             .SDcols = cols_to_sum]


# taking unique values for other mkt share variables and store total total quantity, over store. 
# Don't include product mkt share vars or total shelf space here.
product_vars_tounique <- master_shelf[,c(1:7,9,117:250), with=FALSE]
cols_to_unique <- names(product_vars_tounique)[8:142]

# function to sort the unique elements in a column (ascending) and take the last 
# one, which will be the biggest (possibly 0 if, eg that subfam doesn't exist in a particular store)
unique_element = function(vector){
    sort(unique(vector))[length(unique(vector))]
}

# implement the function
product_vars_tounique <- product_vars_tounique[, lapply(.SD, unique_element), by=.(storeID, sub_chain, chain, town, size, community, province),.SDcols = cols_to_unique]

# merge product mkt share and product mkt share cols back together
stores_dummies <- merge(product_sums, product_vars_tounique, by=c("storeID", "sub_chain", "chain", "town", "size", "community", "province"))
rm(product_sums, product_vars_tounique, cols_to_sum, cols_to_unique, master_shelf)


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

rm(dummy_matrix, dummy_vars, i)

saveRDS(stores_dummies, "stores_dummies_mktshareshelf.RData")
rm(stores_dummies)


################################################################################
# with mkt share for sales
################################################################################
# make copy of master for calculating mkt share by sales dummies
master_sales <- as.data.table(as.data.frame(master))

# remove unnecessary cols
master_sales <- master_sales[,c("mkt_product_store", "mkt_subFam_store", 
                                "mkt_Fam_store", "mkt_Grup_store", 
                                "mkt_Flavor_store", "mkt_Type_store", 
                                "mkt_Units_store", "avg_sales_per_day","weights_kg",
                                "total_shelf_space", "total_quantity"):=NULL]


# variables for which we want to create dummy columns
dummy_vars <- c("productID", "subFam", "fam", "grup", "flavor", "type", "units" )

# using Dummy pkg create dummy vars for each product
for (i in dummy_vars) {
    print(i)
    dummy_matrix <- dummy(i, master_sales)
    dummy_matrix <- as.data.table(dummy_matrix)
    master_sales <- cbind(master_sales, dummy_matrix)
    master_sales[,i:=NULL, with=FALSE]
}

rm(dummy_matrix, dummy_vars, i)

# multiply mkt_product_store_sales by all the dummies relating to products: productID
mkt_share = master_sales$mkt_product_store_sales

for (i in seq(16,122)){
    print(i)
    product = master_sales[,i, with = FALSE]
    master_sales[,names(master_sales)[i] := mkt_share*product]
}

# multiply mkt_product_store by all the dummies relating to products: subFam
mkt_share = master_sales$mkt_subFam_store_sales

for (i in seq(123,183)){
    print(i)
    subFam = master_sales[,i, with = FALSE]
    master_sales[,names(master_sales)[i] := mkt_share*subFam]
}

# multiply mkt_product_store by all the dummies relating to products: fam
mkt_share = master_sales$mkt_Fam_store_sales

for (i in seq(184,224)){
    print(i)
    Fam = master_sales[,i, with = FALSE]
    master_sales[,names(master_sales)[i] := mkt_share*Fam]
}

# multiply mkt_product_store by all the dummies relating to products: grup
mkt_share = master_sales$mkt_Grup_store_sales

for (i in seq(225,237)){
    print(i)
    grup = master_sales[,i, with = FALSE]
    master_sales[,names(master_sales)[i] := mkt_share*grup]
}

# multiply mkt_product_store by all the dummies relating to products: flavor
mkt_share = master_sales$mkt_Flavor_store_sales

for (i in seq(238,247)){
    print(i)
    flavor = master_sales[,i, with = FALSE]
    master_sales[,names(master_sales)[i] := mkt_share*flavor]
}

# multiply mkt_product_store by all the dummies relating to products: type
mkt_share = master_sales$mkt_Type_store_sales

for (i in seq(248,250)){
    print(i)
    type = master_sales[,i, with = FALSE]
    master_sales[,names(master_sales)[i] := mkt_share*type]
}

# multiply mkt_product_store by all the dummies relating to products: units
mkt_share = master_sales$mkt_Units_store_sales

for (i in seq(251,256)){
    print(i)
    units = master_sales[,i, with = FALSE]
    master_sales[,names(master_sales)[i] := mkt_share*units]
}


rm(product, subFam, Fam, grup, flavor, type, units, i, mkt_share)


#remove the original mkt_share variables
master_sales[,c("mkt_product_store_sales","mkt_subFam_store_sales","mkt_Fam_store_sales",
                "mkt_Grup_store_sales", "mkt_Flavor_store_sales","mkt_Type_store_sales",
                "mkt_Units_store_sales"):=NULL]


# summing product mkt share over store
# NB. we could also take the unique value that isn't zero (if it exists, otherwise zero). 
# NB. we don't have the total_quantity variable here since we have the store_total_quantity instead.
product_sums <- master_sales[,1:115, with=FALSE]
cols_to_sum = names(product_sums)[9:115] 
product_sums <- product_sums[, lapply(.SD, sum), 
                             by=.(storeID, sub_chain, chain, town, size, community, province),
                             .SDcols = cols_to_sum]

# taking unique values for other mkt share variables and store total quantity, over store. 
# Don't include product mkt share vars here
product_vars_tounique <- master_sales[,c(1:8,116:249), with=FALSE]
cols_to_unique <- names(product_vars_tounique)[8:142]

# function to sort the unique elements in a column (ascending) and take the last 
# one, which will be the biggest (possibly 0 if, eg that subfam doesn't exist in a particular store)
unique_element = function(vector){
    sort(unique(vector))[length(unique(vector))]
}

product_vars_tounique <- product_vars_tounique[, lapply(.SD, unique_element), by=.(storeID, sub_chain, chain, town, size, community, province),.SDcols = cols_to_unique]

# merge product mkt share and product mkt share cols
stores_dummies <- merge(product_sums, product_vars_tounique, by=c("storeID", "sub_chain", "chain", "town", "size", "community", "province"))
rm(product_sums, product_vars_tounique, cols_to_sum, cols_to_unique, master_sales)


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

rm(dummy_matrix, dummy_vars, i)

saveRDS(stores_dummies, "stores_dummies_mktsharesales.RData")
rm(stores_dummies, unique_element)



################################################################################
                        # k-means clustering #
################################################################################
### Clustering 1: all mkt share by sales + total store quantity. No other store dummies. 

# read in data
stores_dummies_mktsharesales <- readRDS("stores_dummies_mktsharesales.RData")

# remove store variables (except total store quantity)
mktsharesales <- stores_dummies_mktsharesales[,1:243, with=FALSE]

# separate storeID into a different vector
storeIDs <- mktsharesales$storeID
mktsharesales[,storeID:=NULL]


# Calculate the within group sum of squares for different k
wss <- (nrow(mktsharesales)-1)*sum(apply(as.data.frame(mktsharesales),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(mktsharesales,centers=i, iter.max=100)$withinss)
}

# plot 
plot(1:20, wss, type="b", 
     xlab="Number of Clusters", 
     ylab="Within groups sum of squares", 
     main="Clustering 1: market share by sales")
# very uneven - bad clustering. Don't continue.

################################################################################
### Clustering 1a: all mkt share by sales but NO total store quantity. No other store dummies. 

# remove store_total_quantity
mktsharesales[, store_total_quantity:=NULL]

# Calculate the within group sum of squares for different k
wss <- (nrow(mktsharesales)-1)*sum(apply(as.data.frame(mktsharesales),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(mktsharesales,centers=i, iter.max=100)$withinss)
}

# plot 
plot(1:20, wss, type="b", 
     xlab="Number of Clusters", 
     ylab="Within groups sum of squares", 
     main="Clustering 1a: market share by sales - no total store quantity")
rm(mktsharesales)
# still very uneven. Don't continue. 


################################################################################
### Clustering 2: all mkt share by shelf space + total store quantity. No other store dummies. 

# read in data
stores_dummies_mktshareshelf <- readRDS("stores_dummies_mktshareshelf.RData")

# remove store variables (except total store quantity)
mktshareshelf <- stores_dummies_mktshareshelf[,1:244, with=FALSE]

# separate storeID into a different vector
storeIDs <- mktshareshelf$storeID
mktshareshelf[,storeID:=NULL]


# Calculate the within group sum of squares for different k
wss <- (nrow(mktshareshelf)-1)*sum(apply(as.data.frame(mktshareshelf),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(mktshareshelf,centers=i, iter.max=100)$withinss)
}

# plot 
plot(1:20, wss, type="b", 
     xlab="Number of Clusters", 
     ylab="Within groups sum of squares", 
     main="Clustering 2: market share by shelf space")
# choose 6 clusters


# Fit 6 clusters
fit <- kmeans(mktshareshelf, 6, iter.max = 100) 
#centroids <- aggregate(mktshareshelf, by=list(fit$cluster), FUN=mean)
clustering2 <- data.frame(storeIDs, fit$cluster)
names(clustering2) <- c("storeID", "clustering2")

#saveRDS(clustering2, "storeID_clustering2.RData")
rm(mktshareshelf)



################################################################################
### Clustering 3: all mkt share by sales and shelf space + total store quantity. No other store dummies. 

# remove store variables from both tables and store_total_quantity from one table
mktsharesales <- stores_dummies_mktsharesales[,1:243, with=FALSE]
mktshareshelf <- stores_dummies_mktshareshelf[,1:244, with=FALSE]
mktshareshelf[,store_total_quantity := NULL]

# order both tables by storeID and then cbind (removing the storeID column)
setorder(mktsharesales, storeID)
setorder(mktshareshelf, storeID)
mktshareshelf[,storeID:=NULL]
mktshare <- cbind(mktsharesales, mktshareshelf)
rm(mktsharesales, mktshareshelf)


# separate storeID into a different vector
storeIDs <- mktshare$storeID
mktshare[,storeID:=NULL]

# Calculate the within group sum of squares for different k
wss <- (nrow(mktshare)-1)*sum(apply(as.data.frame(mktshare),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(mktshare,centers=i, iter.max=100)$withinss)
}

# plot 
plot(1:20, wss, type="b", 
     xlab="Number of Clusters", 
     ylab="Within groups sum of squares", 
     main="Clustering 3: market share by sales and shelf space")
# choose 7 clusters

# Fit 7 clusters
fit <- kmeans(mktshare, 7, iter.max = 100) 
#centroids <- aggregate(mktshareshelf, by=list(fit$cluster), FUN=mean)
clustering3 <- data.frame(storeIDs, fit$cluster)
names(clustering3) <- c("storeID", "clustering3")

#saveRDS(clustering3, "storeID_clustering3.RData")
rm(mktshare)



################################################################################
### Clustering 4: all mkt share by sales and shelf space + total store quantity + store dummies

# remove store variables from both tables and store_total_quantity from one table
mktsharesales <- as.data.table(as.data.frame(stores_dummies_mktsharesales))
mktshareshelf <- stores_dummies_mktshareshelf[,1:244, with=FALSE]
mktshareshelf[,store_total_quantity := NULL]

# order both tables by storeID and then cbind (removing the storeID column)
setorder(mktsharesales, storeID)
setorder(mktshareshelf, storeID)
mktshareshelf[,storeID:=NULL]
mktshare_storedummies <- cbind(mktsharesales, mktshareshelf)
rm(mktsharesales, mktshareshelf)


# separate storeID into a different vector
storeIDs <- mktshare_storedummies$storeID
mktshare_storedummies[,storeID:=NULL]

# Calculate the within group sum of squares for different k
wss <- (nrow(mktshare_storedummies)-1)*sum(apply(as.data.frame(mktshare_storedummies),2,var))

for (i in 2:20) {
    wss[i] <- sum(kmeans(mktshare_storedummies,centers=i, iter.max=100)$withinss)
}

# plot 
plot(1:20, wss, type="b", 
     xlab="Number of Clusters", 
     ylab="Within groups sum of squares", 
     main="Clustering 4: market share by sales and shelf space and store variables")
# choose 7 clusters

# Fit 7 clusters
fit <- kmeans(mktshare_storedummies, 7, iter.max = 100) 
#centroids <- aggregate(mktshareshelf, by=list(fit$cluster), FUN=mean)
names(clustering4) <- c("storeID", "clustering4")

#saveRDS(clustering4, "storeID_clustering4.RData")
rm(mktshare_storedummies)

################################################################################
############## Save the clusterings ############################################
################################################################################

#merge them 
cluster = merge(clustering4,clustering3, by ="storeID")
cluster = merge(cluster,clustering2, by = "storeID")
#change the names so that they represent what the clusters are based on 
names(cluster)=c("storeID","shelf_sales","sales_shelf","shelf_total")
#save it
saveRDS(cluster,"cluster.RData")











#