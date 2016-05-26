## !!!!!!! Somewhere in here we are duplicating 18 rows (possibly multiple times)

# 1. split store_product data into train and test (based on time) 
# 3. then compute the mode for each

#setwd("~/Desktop/BGSE/Term3/MasterProject/GSE")
#setwd("~/BGSE/semester3/kernel/data")

library(data.table)

store_product <- readRDS("store_product.RData")

# split store_product table into train and test
training_total_days <- 329
shelf_train <- store_product[,1:(4+training_total_days-1), with=FALSE] 

#remove store_product
rm(store_product)

# calc total shelf space for each (store, product)
shelf_train[,total_shelf_space := rowSums(shelf_train[,5:ncol(shelf_train), with = FALSE])]


# calc mode of shelf space for each (store, product)
shelf_train[, mode_shelf_space := NA_integer_]

for (i in 1:nrow(shelf_train)) {
    print(i)
    row <- as.numeric(as.vector(shelf_train[i,5:(ncol(shelf_train)-2), with=FALSE]))
    mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
    shelf_train[i,mode_shelf_space := mode]
}


# sum shelf_space for each store on each day
shelf_train_bystore <-as.data.table(as.data.frame(shelf_train))
# shelf_test_bystore <- as.data.table(as.data.frame(shelf_test))

columns <- c("productID","total_days","store_product_ID","total_shelf_space","mode_shelf_space")
shelf_train_bystore <- shelf_train_bystore[,columns:=NULL, with=FALSE]
#shelf_test_bystore <- shelf_test_bystore[,columns:=NULL, with=FALSE]

shelf_train_bystore <- shelf_train_bystore[,lapply(.SD, sum), by=storeID]
#shelf_test_bystore <- shelf_test_bystore[,lapply(.SD, sum), by=storeID]

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

#remove unnecessary tables
rm(shelf_train_bystore)
# rm(shelf_test_bystore)

# match subFam, Fam and group to shelf space table
products <- readRDS("products.RData")
products <- products[,.(productID, subFam, fam, grup)]
shelf_train <- merge(shelf_train, products, by="productID", all.x=TRUE)

############################ SUBFAM STORE 

# sum shelf_space for each store on each day
shelf_train_bysubFam <- as.data.table(as.data.frame(shelf_train))
# shelf_test_bysubFam <- as.data.table(as.data.frame(shelf_test))

columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","fam","grup")
shelf_train_bysubFam <- shelf_train_bysubFam[,columns:= NULL, with=FALSE]
shelf_train_bysubFam <- shelf_train_bysubFam[,lapply(.SD, sum), by=.(subFam,storeID)]

for (i in 1:nrow(shelf_train_bysubFam)) {
    print(".")
    row <- as.numeric(as.vector(shelf_train_bysubFam[i,2:ncol(shelf_train_bysubFam), with=FALSE]))
    mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
    shelf_train_bysubFam[i,mode_subFam_store := mode]
}

#save just in case of crashing
saveRDS(shelf_train_bysubFam,"shelf_train_bysubFam.RData")
saveRDS(shelf_train,"shelf_train.RData")

# merge these values to shelf space data.tables
shelf_train_bysubFam <- shelf_train_bysubFam[,.(storeID, mode_subFam_store, subFam)]
shelf_train <- merge(shelf_train, shelf_train_bysubFam, by=c("storeID","subFam"), all.x = TRUE)
#remove unnecessary table
rm(shelf_train_bysubFam)
shelf_train<- shelf_train[,mkt_subFam_store:=mode_subFam_store/mode_store]


########################### FAM STORE


# sum shelf_space for each store on each day
shelf_train_byFam <- as.data.table(as.data.frame(shelf_train))
# shelf_test_bysubFam <- as.data.table(as.data.frame(shelf_test))

columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","subFam","grup","mkt_subFam_store")
shelf_train_byFam <- shelf_train_byFam[,columns:= NULL, with=FALSE]
shelf_train_byFam <- shelf_train_byFam[,lapply(.SD, sum), by=.(fam,storeID)]

for (i in 1:nrow(shelf_train_byFam)) {
  print(".")
  row <- as.numeric(as.vector(shelf_train_byFam[i,2:ncol(shelf_train_byFam), with=FALSE]))
  mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
  shelf_train_byFam[i,mode_Fam_store := mode]
}

saveRDS(shelf_train_byFam,"shelf_train_byFam.RData")

# merge these values to shelf space data.tables
shelf_train_byFam <- shelf_train_byFam[,.(storeID, mode_Fam_store, fam)]
shelf_train <- merge(shelf_train, shelf_train_byFam, by=c("storeID","fam"), all.x = TRUE)
#remove unnecessary table
rm(shelf_train_byFam)
shelf_train<- shelf_train[,mkt_Fam_store:=mode_Fam_store/mode_store]


########################### Grup STORE


# sum shelf_space for each store on each day
shelf_train_byGrup <- as.data.table(as.data.frame(shelf_train))
# shelf_test_bysubGrup <- as.data.table(as.data.frame(shelf_test))

columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","subFam","fam","mkt_subFam_store","mkt_Fam_store")
shelf_train_byGrup <- shelf_train_byGrup[,columns:= NULL, with=FALSE]
shelf_train_byGrup <- shelf_train_byGrup[,lapply(.SD, sum), by=.(grup,storeID)]

for (i in 1:nrow(shelf_train_byGrup)) {
  print(".")
  row <- as.numeric(as.vector(shelf_train_byGrup[i,2:ncol(shelf_train_byGrup), with=FALSE]))
  mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
  shelf_train_byGrup[i,mode_Grup_store := mode]
}
saveRDS(shelf_train_byGrup,"shelf_train_byGrup.RData")
saveRDS(shelf_train,"shelf_train.RData")

# merge these values to shelf space data.tables
shelf_train_byGrup <- shelf_train_byGrup[,.(storeID, mode_Grup_store, grup)]
shelf_train <- merge(shelf_train, shelf_train_byGrup, by=c("storeID","grup"), all.x = TRUE)
#remove unnecessary table
rm(shelf_train_byGrup)
shelf_train<- shelf_train[,mkt_Grup_store:=mode_Grup_store/mode_store]
saveRDS(shelf_train,"shelf_train.RData")

########################### Flavor STORE


# sum shelf_space for each store on each day
#add flavor to each productID if necessary
products = readRDS("products.RData")
products = products[,.(productID,flavor,type)]
shelf_train = merge(shelf_train,products, by = "productID", all.x=TRUE)
rm(products)

shelf_train_byFlavor <- as.data.table(as.data.frame(shelf_train))


columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","subFam","fam",
             "mkt_subFam_store","mkt_Fam_store","mkt_Grup_store","type")
shelf_train_byFlavor <- shelf_train_byFlavor[,columns:= NULL, with=FALSE]
shelf_train_byFlavor <- shelf_train_byFlavor[,lapply(.SD, sum), by=.(flavor,storeID)]

for (i in 1:nrow(shelf_train_byFlavor)) {
    print(i)
    row <- as.numeric(as.vector(shelf_train_byFlavor[i,3:ncol(shelf_train_byFlavor), with=FALSE]))
    mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
    shelf_train_byFlavor[i,mode_Flavor_store := mode]
}

saveRDS(shelf_train_byGrup,"shelf_train_byGrup.RData")
saveRDS(shelf_train,"shelf_train.RData")

# merge these values to shelf space data.tables
shelf_train_byFlavor <- shelf_train_byFlavor[,.(storeID, mode_Flavor_store, flavor)]
shelf_train <- merge(shelf_train, shelf_train_byFlavor, by=c("storeID","flavor"), all.x = TRUE)
#remove unnecessary table
rm(shelf_train_byFlavor)
shelf_train<- shelf_train[,mkt_Flavor_store:=mode_Flavor_store/mode_store]
saveRDS(shelf_train,"shelf_train_mode_flavor.RData")

########################### Type STORE


# sum shelf_space for each store on each day
#add flavor to each productID if necessary
# products = readRDS("products.RData")
# products = products[,.(productID,flavor,type)]
# shelf_train = merge(shelf_train,products, by = "productID", all.x=TRUE)
# rm(products)

shelf_train_byType <- as.data.table(as.data.frame(shelf_train))


columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","subFam","fam",
             "mkt_subFam_store","mkt_Fam_store","mkt_Grup_store","flavor","mkt_Flavor_store")
shelf_train_byType <- shelf_train_byType[,columns:= NULL, with=FALSE]
shelf_train_byType <- shelf_train_byType[,lapply(.SD, sum), by=.(type,storeID)]

for (i in 1:nrow(shelf_train_byType)) {
    print(i)
    row <- as.numeric(as.vector(shelf_train_byType[i,3:ncol(shelf_train_byType), with=FALSE]))
    mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
    shelf_train_byType[i,mode_Type_store := mode]
}

#saveRDS(shelf_train_byGrup,"shelf_train_byGrup.RData")
#saveRDS(shelf_train,"shelf_train.RData")

# merge these values to shelf space data.tables
shelf_train_byType <- shelf_train_byType[,.(storeID, mode_Type_store, type)]
shelf_train <- merge(shelf_train, shelf_train_byType, by=c("storeID","type"), all.x = TRUE)
#remove unnecessary table
rm(shelf_train_byType)
shelf_train<- shelf_train[,mkt_Type_store:=mode_Type_store/mode_store]
saveRDS(shelf_train,"shelf_train_mode_Type.RData")


#########################################
### MERGE WITH THE MASTER TRAIN TABLE
########################################


## leave only the necessary columns
shelf_train = shelf_train[,.(storeID, productID,total_shelf_space,mode_shelf_space,mkt_product_store,mkt_subFam_store,
                             mkt_Fam_store,mkt_Grup_store)]
master_train = readRDS("master_train_datasplitting.RData")

master_train = merge(x = master_train,y = shelf_train,by=c("storeID","productID"), all.x=TRUE)
saveRDS(master_train,"master_train_mktshare.RData")

