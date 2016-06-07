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


############################ PRODUCT in STORE ##################################
# sum shelf_space for each store on each day
shelf_train_bystore <-as.data.table(as.data.frame(shelf_train))
columns <- c("productID","total_days","store_product_ID","total_shelf_space",
             "mode_shelf_space")
shelf_train_bystore <- shelf_train_bystore[,columns:=NULL, with=FALSE]
shelf_train_bystore <- shelf_train_bystore[,lapply(.SD, sum), by=storeID]

# calc mode of the shelf space of each product in each store
for (i in 1:nrow(shelf_train_bystore)) {
    print(i)
    row <- as.numeric(as.vector(shelf_train_bystore[i,2:ncol(shelf_train_bystore), with=FALSE]))
    mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
    shelf_train_bystore[i,mode_store := mode]
}

# merge these values to shelf space data.tables
shelf_train_bystore <- shelf_train_bystore[,.(storeID, mode_store)]
shelf_train <- merge(shelf_train, shelf_train_bystore, by="storeID", all.x = TRUE)

# create market share of product in store variable
shelf_train <- shelf_train[,mkt_product_store:= mode_shelf_space/mode_store]

#remove unnecessary tables
rm(shelf_train_bystore)


####### Read in product vars to create aggregated market share variables #######
# match subFam, Fam and group to shelf space table
products <- readRDS("products.RData")
products <- products[,.(productID, subFam, fam, grup, flavor, type, units)]
shelf_train <- merge(shelf_train, products, by="productID", all.x=TRUE)

rm(products)


############################ SUBFAM in STORE ###################################
# sum shelf_space for each (sub_fam, store) on each day
shelf_train_bysubFam <- as.data.table(as.data.frame(shelf_train))
columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","fam","grup","flavor", "type", "units")
shelf_train_bysubFam <- shelf_train_bysubFam[,columns:= NULL, with=FALSE]
shelf_train_bysubFam <- shelf_train_bysubFam[,lapply(.SD, sum), by=.(subFam,storeID)]

# calc mode of the shelf space of each subfam in each store
for (i in 1:nrow(shelf_train_bysubFam)) {
    print(i)
    row <- as.numeric(as.vector(shelf_train_bysubFam[i,2:(ncol(shelf_train_bysubFam)-1), with=FALSE]))
    mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
    shelf_train_bysubFam[i, mode_subFam_store := mode]
}

#save just in case of crashing
saveRDS(shelf_train_bysubFam,"temp_shelf_train_bysubFam.RData")
saveRDS(shelf_train,"temp_shelf_train.RData")

# merge these values to shelf space data.tables
shelf_train_bysubFam <- shelf_train_bysubFam[,.(storeID, mode_subFam_store, subFam)]
shelf_train <- merge(shelf_train, shelf_train_bysubFam, by=c("storeID","subFam"), all.x = TRUE)

# create market share of sub_fam in store variable
shelf_train <- shelf_train[,mkt_subFam_store:= mode_subFam_store/mode_store]

#remove unnecessary table
rm(shelf_train_bysubFam)


############################# FAM in STORE #####################################
# sum shelf_space for each (fam, store) on each day
shelf_train_byFam <- as.data.table(as.data.frame(shelf_train))
columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","subFam","grup","flavor", 
             "type","units", "mkt_subFam_store","mode_subFam_store")
shelf_train_byFam <- shelf_train_byFam[,columns:= NULL, with=FALSE]
shelf_train_byFam <- shelf_train_byFam[,lapply(.SD, sum), by=.(fam,storeID)]

# calc mode of the shelf space of each fam in each store
for (i in 1:nrow(shelf_train_byFam)) {
  print(i)
  row <- as.numeric(as.vector(shelf_train_byFam[i,2:(ncol(shelf_train_byFam)-1), with=FALSE]))
  mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
  shelf_train_byFam[i,mode_Fam_store := mode]
}

# save in case of crashing
saveRDS(shelf_train_byFam,"temp_shelf_train_byFam.RData")
saveRDS(shelf_train,"temp_shelf_train.RData")

# merge these values to shelf space data.tables
shelf_train_byFam <- shelf_train_byFam[,.(storeID, mode_Fam_store, fam)]
shelf_train <- merge(shelf_train, shelf_train_byFam, by=c("storeID","fam"), all.x = TRUE)

# create market share of fam in store variable
shelf_train <- shelf_train[,mkt_Fam_store:=mode_Fam_store/mode_store]

#remove unnecessary table
rm(shelf_train_byFam)


############################## GRUP in STORE ###################################
# sum shelf_space for each (group, store) on each day
shelf_train_byGrup <- as.data.table(as.data.frame(shelf_train))
columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","subFam","fam", "flavor", 
             "type", "units", "mkt_subFam_store","mkt_Fam_store","mode_subFam_store","mode_Fam_store" )
shelf_train_byGrup <- shelf_train_byGrup[,columns:= NULL, with=FALSE]
shelf_train_byGrup <- shelf_train_byGrup[,lapply(.SD, sum), by=.(grup,storeID)]

# calc mode of the shelf space of each grup in each store
for (i in 1:nrow(shelf_train_byGrup)) {
  print(i)
  row <- as.numeric(as.vector(shelf_train_byGrup[i,3:ncol(shelf_train_byGrup), with=FALSE]))
  mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
  shelf_train_byGrup[i, mode_Grup_store := mode]
}

# save in case of crashing
saveRDS(shelf_train_byGrup,"temp_shelf_train_byGrup.RData")
saveRDS(shelf_train,"temp_shelf_train.RData")

# merge these values to shelf space data.tables
shelf_train_byGrup <- shelf_train_byGrup[,.(storeID, mode_Grup_store, grup)]
shelf_train <- merge(shelf_train, shelf_train_byGrup, by=c("storeID","grup"), all.x = TRUE)

# create market share of grup in store variable
shelf_train <- shelf_train[,mkt_Grup_store:= mode_Grup_store/mode_store]

#remove unnecessary table
rm(shelf_train_byGrup)


############################## FLAVOR in STORE #################################
# sum shelf_space for each (flavour, store) on each day
shelf_train_byFlavor <- as.data.table(as.data.frame(shelf_train))

columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","subFam","fam", 
             "grup", "type", "units", "mkt_subFam_store","mkt_Fam_store", 
             "mkt_Grup_store","mode_subFam_store","mode_Fam_store","mode_Grup_store")
shelf_train_byFlavor <- shelf_train_byFlavor[,columns:= NULL, with=FALSE]
shelf_train_byFlavor <- shelf_train_byFlavor[,lapply(.SD, sum), by=.(flavor,storeID)]

# calc mode of the shelf space of each flavour in each store
for (i in 1:nrow(shelf_train_byFlavor)) {
    print(i)
    row <- as.numeric(as.vector(shelf_train_byFlavor[i,3:ncol(shelf_train_byFlavor), with=FALSE]))
    mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
    shelf_train_byFlavor[i, mode_Flavor_store := mode]
}

# save in case of crashing
saveRDS(shelf_train_byFlavor,"temp_shelf_train_byFlavor.RData")
saveRDS(shelf_train,"temp_shelf_train.RData")

# merge these values to shelf space data.tables
shelf_train_byFlavor <- shelf_train_byFlavor[,.(storeID, mode_Flavor_store, flavor)]
shelf_train <- merge(shelf_train, shelf_train_byFlavor, by=c("storeID","flavor"), all.x = TRUE)

# create market share of flavor in store variable
shelf_train <- shelf_train[,mkt_Flavor_store:= mode_Flavor_store/mode_store]

#remove unnecessary table
rm(shelf_train_byFlavor)


############################### TYPE in STORE ##################################
# sum shelf_space for each (type, store) on each day
shelf_train_byType <- as.data.table(as.data.frame(shelf_train))
columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","subFam","fam", 
             "grup", "flavor", "units", "mkt_subFam_store","mkt_Fam_store", 
             "mkt_Grup_store", "mkt_Flavor_store","mode_subFam_store","mode_Fam_store",
             "mode_Grup_store","mode_Flavor_store")
shelf_train_byType <- shelf_train_byType[,columns:= NULL, with=FALSE]
shelf_train_byType <- shelf_train_byType[,lapply(.SD, sum), by=.(type,storeID)]

# calc mode of the shelf space of each type in each store
for (i in 1:nrow(shelf_train_byType)) {
    print(i)
    row <- as.numeric(as.vector(shelf_train_byType[i,3:ncol(shelf_train_byType), with=FALSE]))
    mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
    shelf_train_byType[i,mode_Type_store := mode]
}

# save in case of crashing
saveRDS(shelf_train_byType,"temp_shelf_train_byType.RData")
saveRDS(shelf_train,"temp_shelf_train.RData")

# merge these values to shelf space data.tables
shelf_train_byType <- shelf_train_byType[,.(storeID, mode_Type_store, type)]
shelf_train <- merge(shelf_train, shelf_train_byType, by=c("storeID","type"), all.x = TRUE)

# create market share of type in store variable
shelf_train<- shelf_train[,mkt_Type_store:= mode_Type_store/mode_store]

#remove unnecessary table
rm(shelf_train_byType)


############################## UNITS in STORE ##################################
# sum shelf_space for each (units, store) on each day
shelf_train_byUnits <- as.data.table(as.data.frame(shelf_train))
columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","subFam","fam", 
             "grup", "flavor", "type", "mkt_subFam_store","mkt_Fam_store", 
             "mkt_Grup_store", "mkt_Flavor_store", "mkt_Type_store",
             "mode_subFam_store", "mode_Fam_store"    ,"mode_Grup_store"   ,"mode_Flavor_store"
             ,"mode_Type_store")
shelf_train_byUnits <- shelf_train_byUnits[,columns:= NULL, with=FALSE]
shelf_train_byUnits <- shelf_train_byUnits[,lapply(.SD, sum), by=.(units,storeID)]

# calc mode of the shelf space of each units in each store
for (i in 1:nrow(shelf_train_byUnits)) {
    print(i)
    row <- as.numeric(as.vector(shelf_train_byUnits[i,2:(ncol(shelf_train_byUnits)-1), with=FALSE]))
    mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
    shelf_train_byUnits[i,mode_Units_store := mode]
}

# save in case of crashing
saveRDS(shelf_train_byUnits,"temp_shelf_train_byUnits.RData")
saveRDS(shelf_train,"temp_shelf_train.RData")

# merge these values to shelf space data.tables
shelf_train_byUnits <- shelf_train_byUnits[,.(storeID, mode_Units_store, units)]
shelf_train <- merge(shelf_train, shelf_train_byUnits, by=c("storeID","units"), all.x = TRUE)

# create market share of type in store variable
shelf_train <- shelf_train[,mkt_Units_store:= mode_Units_store/mode_store]

#remove unnecessary table
rm(shelf_train_byUnits)



# save shelf train with all mkt shares (by shelf space) calc'd
saveRDS(shelf_train,"temp_shelf_train.RData")


################################################################################
                    # merge with master_train table #
################################################################################
## leave only the necessary columns
shelf_train <- shelf_train[,.(storeID, productID,total_shelf_space,mode_shelf_space,
                             mkt_product_store,mkt_subFam_store,mkt_Fam_store,
                             mkt_Grup_store, mkt_Flavor_store, mkt_Type_store,
                             mkt_Units_store)]
master_train <- readRDS("master_train_datasplitting.RData") # there are 119,101 duplicates in this!!!!

master_train <- merge(master_train, shelf_train, by=c("storeID","productID"), all.x=TRUE)

# save master_train
saveRDS(master_train,"master_train_mktshare.RData")
#saveRDS(master_train,"master_train_mktshare_checked.RData") # this was after rerunning the code to see where the duplicates were being created. 

