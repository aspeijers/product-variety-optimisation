# 1. split store_product data into train and test (based on time) 
# 3. then compute the mode for each

setwd("~/Desktop/BGSE/Term3/MasterProject/GSE")

library(data.table)

store_product <- readRDS("store_product.RData")

# find dates for train and test
training_total_days <- 329
shelf_train <- store_product[,1:(4+training_total_days-1), with=FALSE] 
shelf_test <- cbind(store_product[,.(storeID, productID, total_days, store_product_ID)], 
                     store_product[,(4+training_total_days):(ncol(store_product)-2), with=FALSE])

# calc total shelf space for each (store, product)
shelf_train[,total_shelf_space := rowSums(shelf_train[,5:ncol(shelf_train), with = FALSE])]
shelf_test[,total_shelf_space := rowSums(shelf_test[,5:ncol(shelf_test), with = FALSE])]

# calc mode of shelf space for each (store, product)
shelf_train[, mode_shelf_space := NA_integer_]
shelf_test[, mode_shelf_space := NA_integer_]


for (i in 1:nrow(shelf_train)) {
    print(i)
    row <- as.numeric(as.vector(shelf_train[i,5:(ncol(shelf_train)-2), with=FALSE]))
    mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
    shelf_train[i,mode_shelf_space := mode]
}

for (i in 1:nrow(shelf_test)) {
    print(i)
    row <- as.numeric(as.vector(shelf_test[i,5:(ncol(shelf_test)-2), with=FALSE]))
    mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
    shelf_test[i,mode_shelf_space := mode]
}


########################## try mode method
# sum shelf_space for each store on each day
shelf_train_bystore <- shelf_train
#### something is wrong 
shelf_train_bystore <- shelf_train_bystore[,c("productID", "total_days", "store_product_ID", "total_shelf_space", "mode_shelf_space"):= NULL]
shelf_train_bystore <- shelf_train_bystore[,lapply(.SD, sum), by=storeID]

for (i in 1:nrow(shelf_train_bystore)) {
    print(i)
    row <- as.numeric(as.vector(shelf_train_bystore[i,2:ncol(shelf_train_bystore), with=FALSE]))
    mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
    shelf_train_bystore[i,mode_store := mode]
}

# merge these values to shelf space data.tables
shelf_train_bystore <- shelf_train_bystore[,.(storeID, mode_store)]
shelf_train <- merge(shelf_train, shelf_train_bystore, by="storeID", all.x = TRUE)
shelf_train<- shelf_train[,mkt_product_store:=mode_shelf_space/mode_store]

# match subFam, Fam and group to shelf space table
products <- readRDS("products.RData")
products <- products[,.(productID, subFam, fam, grup)]
shelf_train <- merge(shelf_train, products, by="productID", all.x=TRUE)

# sum shelf space for each subFam for each store 

# sum shelf_space for each store on each day
shelf_train_bysubFam <- shelf_train
shelf_train_bysubFam <- shelf_train_bysubFam[,c("productID", "total_days", "store_product_ID", "total_shelf_space", 
                                                "mode_shelf_space","mode_store","mkt_product_store","fam","grup"):= NULL]
shelf_train_bysubFam <- shelf_train_bysubFam[,lapply(.SD, sum), by=.(subFam,storeID)]

for (i in 1:nrow(shelf_train_bysubFam)) {
    print(i)
    row <- as.numeric(as.vector(shelf_train_bysubFam[i,2:ncol(shelf_train_bysubFam), with=FALSE]))
    mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
    shelf_train_bysubFam[i,mode_subFam_store := mode]
}

# merge these values to shelf space data.tables
shelf_train_bysubFam <- shelf_train_bysubFam[,.(storeID, mode_subFam_store, subFam)]
shelf_train <- merge(shelf_train, shelf_train_bysubFam, by=c("storeID","subFam"), all.x = TRUE)
shelf_train<- shelf_train[,mkt_subFam_store:=mode_subFam_store/mode_store]



