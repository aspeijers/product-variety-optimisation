## !!!!!!!! Somewhere in here we are duplicating 18 rows (possibly multiple times)
## !!!!!!! Also should the mode loop be starting at col 3 instead of 2?

# 1. split store_product data into train and test (based on time) 
# 2. then compute the mode for each

#setwd("~/Desktop/BGSE/Term3/MasterProject/GSE")
#setwd("~/BGSE/semester3/kernel/data")

library(data.table)

store_product <- readRDS("store_product.RData")

# split store_product table into train and test
training_total_days <- 329
shelf_test <- cbind(store_product[,.(storeID, productID, total_days, store_product_ID)], 
                     store_product[,(4+training_total_days):ncol(store_product), with=FALSE])

#remove store_product
rm(store_product)

# calc total shelf space for each (store, product)
shelf_test[,total_shelf_space := rowSums(shelf_test[,5:ncol(shelf_test), with = FALSE])]


# calc mode of shelf space for each (store, product)
shelf_test[, mode_shelf_space := NA_integer_]

for (i in 1:nrow(shelf_test)) {
    print(i)
    row <- as.numeric(as.vector(shelf_test[i,5:(ncol(shelf_test)-2), with=FALSE]))
    mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
    shelf_test[i,mode_shelf_space := mode]
}


# sum shelf_space for each store on each day
shelf_test_bystore <-as.data.table(as.data.frame(shelf_test))

columns <- c("productID","total_days","store_product_ID","total_shelf_space","mode_shelf_space")
shelf_test_bystore <- shelf_test_bystore[,columns:=NULL, with=FALSE]

shelf_test_bystore <- shelf_test_bystore[,lapply(.SD, sum), by=storeID]

for (i in 1:nrow(shelf_test_bystore)) {
    print(i)
    row <- as.numeric(as.vector(shelf_test_bystore[i,2:ncol(shelf_test_bystore), with=FALSE]))
    mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
    shelf_test_bystore[i,mode_store := mode]
}


# merge these values to shelf space data.tables
shelf_test_bystore <- shelf_test_bystore[,.(storeID, mode_store)]
shelf_test <- merge(shelf_test, shelf_test_bystore, by="storeID", all.x = TRUE)
shelf_test<- shelf_test[,mkt_product_store:=mode_shelf_space/mode_store]

#remove unnecessary tables
rm(shelf_test_bystore)

################# match subFam, Fam and group to shelf space table #############
products <- readRDS("products.RData")
products <- products[,.(productID, subFam, fam, grup, flavor, type, units)]
shelf_test <- merge(shelf_test, products, by="productID", all.x=TRUE)

############################ SUBFAM STORE 

# sum shelf_space for each store on each day
shelf_test_bysubFam <- as.data.table(as.data.frame(shelf_test))

columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","fam","grup","flavor", "type", "units")
shelf_test_bysubFam <- shelf_test_bysubFam[,columns:= NULL, with=FALSE]
shelf_test_bysubFam <- shelf_test_bysubFam[,lapply(.SD, sum), by=.(subFam,storeID)]

for (i in 1:nrow(shelf_test_bysubFam)) {
    print(".")
    row <- as.numeric(as.vector(shelf_test_bysubFam[i,2:ncol(shelf_test_bysubFam), with=FALSE]))
    mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
    shelf_test_bysubFam[i,mode_subFam_store := mode]
}

#save just in case of crashing
saveRDS(shelf_test_bysubFam,"shelf_test_bysubFam.RData")
saveRDS(shelf_test,"shelf_test.RData")

# merge these values to shelf space data.tables
shelf_test_bysubFam <- shelf_test_bysubFam[,.(storeID, mode_subFam_store, subFam)]
shelf_test <- merge(shelf_test, shelf_test_bysubFam, by=c("storeID","subFam"), all.x = TRUE)
#remove unnecessary table
rm(shelf_test_bysubFam)
shelf_test<- shelf_test[,mkt_subFam_store:=mode_subFam_store/mode_store]


########################### FAM STORE


# sum shelf_space for each store on each day
shelf_test_byFam <- as.data.table(as.data.frame(shelf_test))

columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","subFam","grup","flavor", 
             "type","units", "mkt_subFam_store","mode_subFam_store")
shelf_test_byFam <- shelf_test_byFam[,columns:= NULL, with=FALSE]
shelf_test_byFam <- shelf_test_byFam[,lapply(.SD, sum), by=.(fam,storeID)]

for (i in 1:nrow(shelf_test_byFam)) {
  print(".")
  row <- as.numeric(as.vector(shelf_test_byFam[i,2:ncol(shelf_test_byFam), with=FALSE]))
  mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
  shelf_test_byFam[i,mode_Fam_store := mode]
}

saveRDS(shelf_test_byFam,"shelf_test_byFam.RData")

# merge these values to shelf space data.tables
shelf_test_byFam <- shelf_test_byFam[,.(storeID, mode_Fam_store, fam)]
shelf_test <- merge(shelf_test, shelf_test_byFam, by=c("storeID","fam"), all.x = TRUE)
#remove unnecessary table
rm(shelf_test_byFam)
shelf_test<- shelf_test[,mkt_Fam_store:=mode_Fam_store/mode_store]


########################### Grup STORE


# sum shelf_space for each store on each day
shelf_test_byGrup <- as.data.table(as.data.frame(shelf_test))

columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","subFam","fam", "flavor", "type", "units", "mkt_subFam_store","mkt_Fam_store","mode_subFam_store","mode_Fam_store" )
shelf_test_byGrup <- shelf_test_byGrup[,columns:= NULL, with=FALSE]
shelf_test_byGrup <- shelf_test_byGrup[,lapply(.SD, sum), by=.(grup,storeID)]

for (i in 1:nrow(shelf_test_byGrup)) {
  print(".")
  row <- as.numeric(as.vector(shelf_test_byGrup[i,2:ncol(shelf_test_byGrup), with=FALSE]))
  mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
  shelf_test_byGrup[i,mode_Grup_store := mode]
}
saveRDS(shelf_test_byGrup,"shelf_test_byGrup.RData")
saveRDS(shelf_test,"shelf_test.RData")

# merge these values to shelf space data.tables
shelf_test_byGrup <- shelf_test_byGrup[,.(storeID, mode_Grup_store, grup)]
shelf_test <- merge(shelf_test, shelf_test_byGrup, by=c("storeID","grup"), all.x = TRUE)
#remove unnecessary table
rm(shelf_test_byGrup)
shelf_test<- shelf_test[,mkt_Grup_store:=mode_Grup_store/mode_store]
saveRDS(shelf_test,"shelf_test.RData")


########################### Flavor STORE


# sum shelf_space for each store on each day
shelf_test_byFlavor <- as.data.table(as.data.frame(shelf_test))

columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","subFam","fam", 
             "grup", "type", "units", "mkt_subFam_store","mkt_Fam_store", 
             "mkt_Grup_store","mode_subFam_store","mode_Fam_store","mode_Grup_store")
shelf_test_byFlavor <- shelf_test_byFlavor[,columns:= NULL, with=FALSE]
shelf_test_byFlavor <- shelf_test_byFlavor[,lapply(.SD, sum), by=.(flavor,storeID)]

for (i in 1:nrow(shelf_test_byFlavor)) {
    print(i)
    row <- as.numeric(as.vector(shelf_test_byFlavor[i,2:ncol(shelf_test_byFlavor), with=FALSE]))
    mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
    shelf_test_byFlavor[i,mode_Flavor_store := mode]
}

saveRDS(shelf_test_byFlavor,"shelf_test_byFlavor.RData")
saveRDS(shelf_test,"shelf_test.RData")

# merge these values to shelf space data.tables
shelf_test_byFlavor <- shelf_test_byFlavor[,.(storeID, mode_Flavor_store, flavor)]
shelf_test <- merge(shelf_test, shelf_test_byFlavor, by=c("storeID","flavor"), all.x = TRUE)
#remove unnecessary table
rm(shelf_test_byFlavor)
shelf_test<- shelf_test[,mkt_Flavor_store:=mode_Flavor_store/mode_store]
saveRDS(shelf_test,"shelf_test.RData")


########################### Type STORE


# sum shelf_space for each store on each day
shelf_test_byType <- as.data.table(as.data.frame(shelf_test))

columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","subFam","fam", 
             "grup", "flavor", "units", "mkt_subFam_store","mkt_Fam_store", 
             "mkt_Grup_store", "mkt_Flavor_store","mode_subFam_store","mode_Fam_store",
             "mode_Grup_store","mode_Flavor_store")

shelf_test_byType <- shelf_test_byType[,columns:= NULL, with=FALSE]
shelf_test_byType <- shelf_test_byType[,lapply(.SD, sum), by=.(type,storeID)]

for (i in 1:nrow(shelf_test_byType)) {
    print(i)
    row <- as.numeric(as.vector(shelf_test_byType[i,2:ncol(shelf_test_byType), with=FALSE]))
    mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
    shelf_test_byType[i,mode_Type_store := mode]
}

saveRDS(shelf_test_byType,"shelf_test_byType.RData")
saveRDS(shelf_test,"shelf_test.RData")

# merge these values to shelf space data.tables
shelf_test_byType <- shelf_test_byType[,.(storeID, mode_Type_store, type)]
shelf_test <- merge(shelf_test, shelf_test_byType, by=c("storeID","type"), all.x = TRUE)
#remove unnecessary table
rm(shelf_test_byType)
shelf_test<- shelf_test[,mkt_Type_store:=mode_Type_store/mode_store]
saveRDS(shelf_test,"shelf_test.RData")

########################### Units STORE


# sum shelf_space for each store on each day
shelf_test_byUnits <- as.data.table(as.data.frame(shelf_test))

columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","subFam","fam", 
             "grup", "flavor", "type", "mkt_subFam_store","mkt_Fam_store", 
             "mkt_Grup_store", "mkt_Flavor_store", "mkt_Type_store",
             "mode_subFam_store", "mode_Fam_store"    ,"mode_Grup_store"   ,"mode_Flavor_store"
             ,"mode_Type_store")

shelf_test_byUnits <- shelf_test_byUnits[,columns:= NULL, with=FALSE]
shelf_test_byUnits <- shelf_test_byUnits[,lapply(.SD, sum), by=.(units,storeID)]

for (i in 1:nrow(shelf_test_byUnits)) {
    print(i)
    row <- as.numeric(as.vector(shelf_test_byUnits[i,2:ncol(shelf_test_byUnits), with=FALSE]))
    mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
    shelf_test_byUnits[i,mode_Units_store := mode]
}

saveRDS(shelf_test_byUnits,"shelf_test_byUnits.RData")
saveRDS(shelf_test,"shelf_test.RData")

# merge these values to shelf space data.tables
shelf_test_byUnits <- shelf_test_byUnits[,.(storeID, mode_Units_store, units)]
shelf_test <- merge(shelf_test, shelf_test_byUnits, by=c("storeID","units"), all.x = TRUE)
#remove unnecessary table
rm(shelf_test_byUnits)
shelf_test<- shelf_test[,mkt_Units_store:=mode_Units_store/mode_store]
saveRDS(shelf_test,"shelf_test.RData")

#########################################
### MERGE WITH THE MASTER TRAIN TABLE
########################################


## leave only the necessary columns
shelf_test <- shelf_test[,.(storeID, productID, total_shelf_space, mode_shelf_space, mkt_product_store, mkt_subFam_store,
                             mkt_Fam_store, mkt_Grup_store, mkt_Flavor_store, mkt_Type_store, mkt_Units_store)]
master_test <- readRDS("master_test_datasplitting.RData")

master_test <- merge(x = master_test,y = shelf_test, by=c("storeID","productID"), all.x=TRUE)

saveRDS(master_test,"master_test_mktshare.RData")

