# This file is split into the following sections. 
# 1) Convert all variables in the master table to dummy variables for each store
# 2) Cluster the stores based on various subsets of these variables. 
# nb. subsets have been chosen via logical reasoning. Clustering with other variable
# subsets is done in the file "store_clustering_1.Rmd"

# load packages
library(data.table)
library(dummies)
library(ggplot2)
library(grid)
library(gridExtra)

################################################################################
                    # 1) Create stores_dummies table #
################################################################################
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
# Create mkt share by shelf space dummies

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

# standardise store total shelf space variable
stores_dummies_mktshareshelf <- readRDS("stores_dummies_mktshareshelf.RData")
#min_ss <- min(stores_dummies_mktshareshelf$total_shelf_space)
#max_ss <- max(stores_dummies_mktshareshelf$total_shelf_space)
#stores_dummies_mktshareshelf$total_shelf_space <- (stores_dummies_mktshareshelf$total_shelf_space - min_ss)/(max_ss - min_ss)
stores_dummies_mktshareshelf$total_shelf_space <- scale(stores_dummies_mktshareshelf$total_shelf_space)
plot(stores_dummies_mktshareshelf$total_shelf_space)
saveRDS(stores_dummies_mktshareshelf, "stores_dummies_mktshareshelf.RData")

################################################################################
# Create market share by sell-in dummies 

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
                        # 2) k-means clustering #
################################################################################
# read in data
stores_dummies_mktsharesales <- readRDS("stores_dummies_mktsharesales.RData")
stores_dummies_mktshareshelf <- readRDS("stores_dummies_mktshareshelf.RData")

# remove store dummy and total sell-in columns from one table (since they are common to both)
stores_dummies_mktshareshelf <- stores_dummies_mktshareshelf[,1:244, with=FALSE]
stores_dummies_mktshareshelf[,store_total_quantity:=NULL]

# rename shelf space columns
names(stores_dummies_mktshareshelf)[3:ncol(stores_dummies_mktshareshelf)] <- paste0(names(stores_dummies_mktshareshelf)[3:ncol(stores_dummies_mktshareshelf)], "_ss")

# combine data into one table
stores_dummies_mktshares <- merge(stores_dummies_mktsharesales, stores_dummies_mktshareshelf, by="storeID") 
rm(stores_dummies_mktsharesales, stores_dummies_mktshareshelf)

# write function to produce clusters and plot
# plot_title is a string stating which variables are included in the clustering and which are not
# features is a dataframe where each column is a store variable.
clustering <- function( plot_title, features, max_clusters=20, max_iters=100 ) {
    
    # Calculate the within group sum of squares for different k
    wss <- (nrow(features)-1)*sum(apply(as.data.frame(features),2,var))
    
    for (i in 2:20) {
        wss[i] <- sum( kmeans(features, centers=i, iter.max=max_iters)$withinss )
    }
    
    df <- data.frame(Clusters = 1:20, wwss=wss)
    clust_plot <- ggplot(data=df, aes(x=Clusters, y=wwss, group=1)) + 
        geom_line(colour="dodgerblue2") +
        geom_point(colour="dodgerblue2", size=3 ) +
        #geom_vline(xintercept = 7, linetype=4) +
        theme_bw() +
        theme(plot.title = element_text(size = rel(1.5)),
              axis.title.y = element_text(size = rel(1.5)),
              axis.title.x = element_text(size = rel(1.5)),
              axis.text.x  = element_text(size= rel(1.2)),
              axis.text.y = element_text(size= rel(1.2)) ) +
        ylab("Within groups sum of squares") +
        xlab("Number of clusters") +
        ggtitle( paste0("Clustering variables: \n", plot_title) ) 
    
    return(list(clusters=df, plot=clust_plot))
    
}

# save storeID's then remove from dataset
storeIDs <- as.integer(as.character(stores_dummies_mktshares$storeID))
stores_dummies_mktshares[,storeID:=NULL]

# Cluster
# all mkt share by sell-in variables
title1  <- "mkt share sell-in (no total sell-in or store dummies)"
feat1   <- stores_dummies_mktshares[,1:242, with=FALSE]
feat1   <- feat1[,store_total_quantity:=NULL]
clust1  <- clustering( title1, feat1 )

title2  <- "mkt share sell-in with total sell-in (no store dummies)"
feat2   <- stores_dummies_mktshares[,1:242, with=FALSE]
clust2  <- clustering( title2, feat2 )

title3  <- "mkt share sell-in with store dummies (no total sell-in)"
feat3   <- stores_dummies_mktshares[,1:1111, with=FALSE]
feat3   <- feat3[,store_total_quantity:=NULL]
clust3  <- clustering( title3, feat3 )

title4  <- "mkt share sell-in with total sell-in and store dummies"
feat4   <- stores_dummies_mktshares[,1:1111, with=FALSE]
clust4  <- clustering( title4, feat4 )

# all mkt share by sell-in AND all mkt share by shelf space variables - no store dummies
title9  <- "mkt share sell-in and shelfspace (no total sell-in, total ss or store dummies)"
feat9   <- stores_dummies_mktshares[,c(1:242, 1112:1353), with=FALSE]
feat9   <- feat9[,c("store_total_quantity", "total_shelf_space"):=NULL]
clust9  <- clustering( title9, feat9 )

title12  <- "mkt share sell-in and shelfspace with total sell-in and total ss (no store dummies)"
feat12   <- stores_dummies_mktshares[,c(1:242, 1112:1353), with=FALSE]
clust12  <- clustering( title12, feat12 )

# all mkt share by sell-in AND all mkt share by shelf space variables - with store dummies
title13  <- "mkt share sell-in and shelfspace with store dummies (no total sell-in or total ss)"
feat13   <- as.data.table(as.data.frame(stores_dummies_mktshares))
feat13   <- feat13[,c("store_total_quantity", "total_shelf_space"):=NULL]
clust13  <- clustering( title13, feat13 )

title16  <- "mkt share sell-in and shelfspace with total sell-in, total ss and store dummies"
feat16   <- as.data.table(as.data.frame(stores_dummies_mktshares))
clust16  <- clustering( title16, feat16 )

# plot
grid.arrange(clust1$plot, clust2$plot, clust3$plot, clust4$plot, ncol = 2)
grid.arrange(clust9$plot, clust12$plot, clust13$plot, clust16$plot, ncol = 2)

################################################################################
# Check that clusters have more than 1 store.

# calculate store cluster numbers using each of the chosen clusterings
# Nb. These clusterings were chosen by logical reasoning. 
chosen <- c("clust1", "clust2", "clust3", "clust4",
            "clust9", "clust12", "clust13", "clust16")
feats <- list(feat1, feat2, feat3, feat4,
              feat9, feat12, feat13, feat16)

# no of clusters to test
no_clusters <- 5:20

# initialise an empty data frame to fill up
clusters <- as.data.frame( matrix(NA, nrow=length(storeIDs), ncol=length(feats)*length(no_clusters)) )
names(clusters) <- c(paste0(no_clusters[1], "_", chosen),
                     paste0(no_clusters[2], "_", chosen),
                     paste0(no_clusters[3], "_", chosen),
                     paste0(no_clusters[4], "_", chosen),
                     paste0(no_clusters[5], "_", chosen),
                     paste0(no_clusters[6], "_", chosen),
                     paste0(no_clusters[7], "_", chosen),
                     paste0(no_clusters[8], "_", chosen),
                     paste0(no_clusters[9], "_", chosen),
                     paste0(no_clusters[10], "_", chosen),
                     paste0(no_clusters[11], "_", chosen),
                     paste0(no_clusters[12], "_", chosen),
                     paste0(no_clusters[13], "_", chosen),
                     paste0(no_clusters[14], "_", chosen),
                     paste0(no_clusters[15], "_", chosen),
                     paste0(no_clusters[16], "_", chosen) )

# for each of the chosen clusterings and no of clusters, produce the store cluster numbers
for (k in 1:length(no_clusters)) {
    print(k)
    for (i in 1:length(chosen)) {
        fit <- kmeans( feats[[i]], no_clusters[k], iter.max=100)
        column <- (k-1)*length(chosen) + i
        clusters[,column] <- fit$cluster
    }
}

# add storeID's to data frame
clusters <- cbind(storeIDs, clusters)
names(clusters)[1] <- "storeID"

# first run analysis: how many stores are in each cluster? 
for ( i in 2:ncol(clusters) ) {
    cat("column ", i, " min no of stores in cluster is ", min(table(clusters[,i])))
    cat("\n")
}

# find clusterings for which there is a cluster with only 1 store
cols_to_remove <- c()
for ( i in 2:ncol(clusters) ) {
    if ( min(table(clusters[,i])) < 2) {
        cols_to_remove <- c(cols_to_remove, i)
    }
}

# remove clusterings for which there is a cluster with only 1 store
clusters[cols_to_remove] <- list(NULL) 

# save
saveRDS(clusters, "clusterings_1.RData")


################################################################################
# Check that every product is in each cluster 

# read in master table
master <- readRDS("master_mktshare.RData")

# For each clustering, match the cluster numbers to the master table and count 
# the number of unique products that exist in each cluster. 
# There should be 107 (total number of alltimer products) in each cluster.
clust_prods <- merge(master[,.(storeID, productID)], clusters, by="storeID", all.x=TRUE)
names(clust_prods) <- paste0("n",names(clust_prods))
rm(master)

cols_to_remove2 <- c()
for ( i in 3:ncol(clust_prods) ) {
    
    # get column name
    cl <- names(clust_prods)[i]
    
    # count number of unique prods in each cluster
    prods_per_clust <- clust_prods[,.(no_prods = length(unique(nproductID))), by=cl]
    
    # if less than 107 products in each cluster return column name
    if ( min(prods_per_clust$no_prods) < 107 ) {
        cat(cl, "  min products per cluster is: ", min(prods_per_clust$no_prods), "\n")
        cols_to_remove2 <- c(cols_to_remove2, i)
    }
}

# remove clusterings where all 107 products do not appear in every cluster
clusters[(cols_to_remove2-1)] <- list(NULL) 

# save
saveRDS(clusters, "clusterings_2.RData")