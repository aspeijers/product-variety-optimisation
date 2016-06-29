##################################################################################################
######## Description: Compare market share for each product with respect to sell-in and shelf space data  
######## input: stores_dummies_mktsharesales.RData
######## output: 
##################################################################################################

library(data.table)

stores_dummies <- readRDS("stores_dummies_mktsharesales.RData")

product_cols    <- names(stores_dummies)[4:110]
flavor_cols     <- names(stores_dummies)[111:120]
type_cols       <- names(stores_dummies)[121:123]
units_cols      <- names(stores_dummies)[124:129]
grup_cols       <- names(stores_dummies)[130:142]
fam_cols        <- names(stores_dummies)[143:183]
subfam_cols     <- names(stores_dummies)[184:244]

# check products
#store_counts <- rep(0, ncol(stores_dummies))
#names(store_counts) <- as.integer(as.character(stores_dummies$storeID))


# make this a matrix ...
for (i in 1:length(product_cols)) {
    col <- as.data.frame(stores_dummies[,c("storeID", product_cols[i]), with=FALSE])
    col_nonzero <- col[col[,2]>0,2]
    avg <- mean(col_nonzero)
    stdev <- sd(col_nonzero)
    interval_lower <- avg - 3*stdev
    interval_upper <- avg + 3*stdev
    outside_CI <- (col_nonzero<interval_lower | col_nonzero>interval_upper)
    print(c("no_outside CI: ", sum(outside_CI)))
}

# subfam
for (i in 1:length(subfam_cols)) {
    col <- as.data.frame(stores_dummies[,subfam_cols[i], with=FALSE])
    col_nonzero <- col[col>0,1]
    avg <- mean(col_nonzero)
    stdev <- sd(col_nonzero)
    interval_lower <- avg - 3*stdev
    interval_upper <- avg + 3*stdev
    outside_CI <- (col_nonzero<interval_lower | col_nonzero>interval_upper)
    print(c("no_outside CI: ", sum(outside_CI)))
}


# fam
for (i in 1:length(fam_cols)) {
    col <- as.data.frame(stores_dummies[,fam_cols[i], with=FALSE])
    col_nonzero <- col[col>0,1]
    avg <- mean(col_nonzero)
    stdev <- sd(col_nonzero)
    interval_lower <- avg - 3*stdev
    interval_upper <- avg + 3*stdev
    outside_CI <- (col_nonzero<interval_lower | col_nonzero>interval_upper)
    print(c("no_outside CI: ", sum(outside_CI)))
}

# grup
for (i in 1:length(grup_cols)) {
    col <- as.data.frame(stores_dummies[,grup_cols[i], with=FALSE])
    col_nonzero <- col[col>0,1]
    avg <- mean(col_nonzero)
    stdev <- sd(col_nonzero)
    interval_lower <- avg - 3*stdev
    interval_upper <- avg + 3*stdev
    outside_CI <- (col_nonzero<interval_lower | col_nonzero>interval_upper)
    #print(i)
    print(c("no_outside CI: ", sum(outside_CI)))
}