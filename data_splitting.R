library(data.table)
library(plyr)

#setwd("/media/balint/Storage/Tanulas/thesis/product-variety-optimisation")
# setwd("~/Desktop/BGSE/Term3/MasterProject/GSE")

########################## Split by stores ###################################
# read in master table
master <- readRDS("master_mktshare.RData")
master <- master[,storeprodID:=paste0(storeID, productID)]

# split into test and train by (storeID, productID), using representative sampling method (for stores)
set.seed(12345)
test    <- ddply(master, .(sub_chain, chain, town, size, community, province), function(d) { d[sample(nrow(d), pmin(nrow(d), 20)), ]})
test <- as.data.table(test)

train_rows   <- setdiff(master[,storeprodID], test[,storeprodID])
train <- master[storeprodID %in% train_rows]
rm(train_rows)

# remove extra column and master table
rm(master)
test[,storeprodID:=NULL]
train[,storeprodID:=NULL]

# save files
saveRDS(test, "store_test.RData")
saveRDS(train, "store_train.RData")





