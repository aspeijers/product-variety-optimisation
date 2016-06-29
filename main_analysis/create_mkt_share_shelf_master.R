##################################################################################################
######## Description: Creatng the master table with shelf space market shares   
######## input: store_product.RData, product.RData,master_datasplitting.RData
######## output: master_mktshare.RData
################################################################################################## 

library(data.table)

shelf <- readRDS("store_product.RData")

#we already have mode_shelf_space and total_shelf_space

# sum shelf_space for each store on each day
shelf_bystore <-as.data.table(as.data.frame(shelf))

columns <- c("productID","total_days","store_product_ID","total_shelf_space","mode_shelf_space")
shelf_bystore <- shelf_bystore[,columns:=NULL, with=FALSE]

shelf_bystore <- shelf_bystore[,lapply(.SD, sum), by=storeID]

for (i in 1:nrow(shelf_bystore)) {
  print(i)
  row <- as.numeric(as.vector(shelf_bystore[i,2:ncol(shelf_bystore), with=FALSE]))
  mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
  shelf_bystore[i,mode_store := mode]
}


# merge these values to shelf space data.tables
shelf_bystore <- shelf_bystore[,.(storeID, mode_store)]
shelf <- merge(shelf, shelf_bystore, by="storeID", all.x = TRUE)
shelf<- shelf[,mkt_product_store:=mode_shelf_space/mode_store]

#remove unnecessary tables
rm(shelf_bystore)

################# match subFam, Fam and group to shelf space table #############
products <- readRDS("products.RData")
products <- products[,.(productID, subFam, fam, grup, flavor, type, units)]
shelf <- merge(shelf, products, by="productID", all.x=TRUE)

############################ SUBFAM STORE 

# sum shelf_space for each store on each day
shelf_bysubFam <- as.data.table(as.data.frame(shelf))

columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","fam","grup","flavor", "type", "units")
shelf_bysubFam <- shelf_bysubFam[,columns:= NULL, with=FALSE]
shelf_bysubFam <- shelf_bysubFam[,lapply(.SD, sum), by=.(subFam,storeID)]

for (i in 1:nrow(shelf_bysubFam)) {
  print(i)
  row <- as.numeric(as.vector(shelf_bysubFam[i,2:ncol(shelf_bysubFam), with=FALSE]))
  mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
  shelf_bysubFam[i,mode_subFam_store := mode]
}

#save just in case of crashing
saveRDS(shelf_bysubFam,"shelf_bysubFam.RData")
saveRDS(shelf,"shelf.RData")

# merge these values to shelf space data.tables
shelf_bysubFam <- shelf_bysubFam[,.(storeID, mode_subFam_store, subFam)]
shelf <- merge(shelf, shelf_bysubFam, by=c("storeID","subFam"), all.x = TRUE)
#remove unnecessary table
rm(shelf_bysubFam)
shelf<- shelf[,mkt_subFam_store:=mode_subFam_store/mode_store]


########################### FAM STORE


# sum shelf_space for each store on each day
shelf_byFam <- as.data.table(as.data.frame(shelf))

columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","subFam","grup","flavor", "type","units", "mkt_subFam_store","mode_subFam_store")
shelf_byFam <- shelf_byFam[,columns:= NULL, with=FALSE]
shelf_byFam <- shelf_byFam[,lapply(.SD, sum), by=.(fam,storeID)]

for (i in 1:nrow(shelf_byFam)) {
  print(i)
  row <- as.numeric(as.vector(shelf_byFam[i,2:ncol(shelf_byFam), with=FALSE]))
  mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
  shelf_byFam[i,mode_Fam_store := mode]
}

saveRDS(shelf_byFam,"shelf_byFam.RData")

# merge these values to shelf space data.tables
shelf_byFam <- shelf_byFam[,.(storeID, mode_Fam_store, fam)]
shelf <- merge(shelf, shelf_byFam, by=c("storeID","fam"), all.x = TRUE)
#remove unnecessary table
rm(shelf_byFam)
shelf<- shelf[,mkt_Fam_store:=mode_Fam_store/mode_store]


########################### Grup STORE


# sum shelf_space for each store on each day
shelf_byGrup <- as.data.table(as.data.frame(shelf))

columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","subFam","fam", "flavor", "type", "units", "mkt_subFam_store","mkt_Fam_store","mode_subFam_store","mode_Fam_store" )
shelf_byGrup <- shelf_byGrup[,columns:= NULL, with=FALSE]
shelf_byGrup <- shelf_byGrup[,lapply(.SD, sum), by=.(grup,storeID)]

for (i in 1:nrow(shelf_byGrup)) {
  print(i)
  row <- as.numeric(as.vector(shelf_byGrup[i,2:ncol(shelf_byGrup), with=FALSE]))
  mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
  shelf_byGrup[i,mode_Grup_store := mode]
}
saveRDS(shelf_byGrup,"shelf_byGrup.RData")
saveRDS(shelf,"shelf.RData")

# merge these values to shelf space data.tables
shelf_byGrup <- shelf_byGrup[,.(storeID, mode_Grup_store, grup)]
shelf <- merge(shelf, shelf_byGrup, by=c("storeID","grup"), all.x = TRUE)
#remove unnecessary table
rm(shelf_byGrup)
shelf<- shelf[,mkt_Grup_store:=mode_Grup_store/mode_store]
saveRDS(shelf_test,"shelf_test.RData")


########################### Flavor STORE


# sum shelf_space for each store on each day
shelf_byFlavor <- as.data.table(as.data.frame(shelf))

columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","subFam","fam", 
             "grup", "type", "units", "mkt_subFam_store","mkt_Fam_store", 
             "mkt_Grup_store","mode_subFam_store","mode_Fam_store","mode_Grup_store")
shelf_byFlavor <- shelf_byFlavor[,columns:= NULL, with=FALSE]
shelf_byFlavor <- shelf_byFlavor[,lapply(.SD, sum), by=.(flavor,storeID)]

for (i in 1:nrow(shelf_byFlavor)) {
  print(i)
  row <- as.numeric(as.vector(shelf_byFlavor[i,2:ncol(shelf_byFlavor), with=FALSE]))
  mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
  shelf_byFlavor[i,mode_Flavor_store := mode]
}

saveRDS(shelf_byFlavor,"shelf_byFlavor.RData")
saveRDS(shelf,"shelf.RData")

# merge these values to shelf space data.tables
shelf_byFlavor <- shelf_byFlavor[,.(storeID, mode_Flavor_store, flavor)]
shelf <- merge(shelf, shelf_byFlavor, by=c("storeID","flavor"), all.x = TRUE)
#remove unnecessary table
rm(shelf_byFlavor)
shelf<- shelf[,mkt_Flavor_store:=mode_Flavor_store/mode_store]
saveRDS(shelf,"shelf.RData")


########################### Type STORE


# sum shelf_space for each store on each day
shelf_byType <- as.data.table(as.data.frame(shelf))

columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","subFam","fam", 
             "grup", "flavor", "units", "mkt_subFam_store","mkt_Fam_store", 
             "mkt_Grup_store", "mkt_Flavor_store","mode_subFam_store","mode_Fam_store",
             "mode_Grup_store","mode_Flavor_store")

shelf_byType <- shelf_byType[,columns:= NULL, with=FALSE]
shelf_byType <- shelf_byType[,lapply(.SD, sum), by=.(type,storeID)]

for (i in 1:nrow(shelf_byType)) {
  print(i)
  row <- as.numeric(as.vector(shelf_byType[i,2:ncol(shelf_byType), with=FALSE]))
  mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
  shelf_byType[i,mode_Type_store := mode]
}

saveRDS(shelf_byType,"shelf_byType.RData")
saveRDS(shelf,"shelf.RData")

# merge these values to shelf space data.tables
shelf_byType <- shelf_byType[,.(storeID, mode_Type_store, type)]
shelf <- merge(shelf, shelf_byType, by=c("storeID","type"), all.x = TRUE)
#remove unnecessary table
rm(shelf_byType)
shelf<- shelf[,mkt_Type_store:=mode_Type_store/mode_store]
saveRDS(shelf,"shelf.RData")

########################### Units STORE


# sum shelf_space for each store on each day
shelf_byUnits <- as.data.table(as.data.frame(shelf))

columns <- c("productID", "total_days", "store_product_ID", "total_shelf_space", 
             "mode_shelf_space","mode_store","mkt_product_store","subFam","fam", 
             "grup", "flavor", "type", "mkt_subFam_store","mkt_Fam_store", 
             "mkt_Grup_store", "mkt_Flavor_store", "mkt_Type_store",
             "mode_subFam_store", "mode_Fam_store"    ,"mode_Grup_store"   ,"mode_Flavor_store"
             ,"mode_Type_store")

shelf_byUnits <- shelf_byUnits[,columns:= NULL, with=FALSE]
shelf_byUnits <- shelf_byUnits[,lapply(.SD, sum), by=.(units,storeID)]

for (i in 1:nrow(shelf_byUnits)) {
  print(i)
  row <- as.numeric(as.vector(shelf_byUnits[i,2:ncol(shelf_byUnits), with=FALSE]))
  mode <- unique(row)[which.max(tabulate(match(row, unique(row))))]
  shelf_byUnits[i,mode_Units_store := mode]
}

saveRDS(shelf_byUnits,"shelf_byUnits.RData")
saveRDS(shelf,"shelf.RData")

# merge these values to shelf space data.tables
shelf_byUnits <- shelf_byUnits[,.(storeID, mode_Units_store, units)]
shelf <- merge(shelf, shelf_byUnits, by=c("storeID","units"), all.x = TRUE)
#remove unnecessary table
rm(shelf_byUnits)
shelf<- shelf[,mkt_Units_store:=mode_Units_store/mode_store]
saveRDS(shelf,"shelf.RData")

#########################################
### MERGE WITH THE MASTER TABLE
########################################

## check for duplicates 
sum(duplicated(shelf$store_product_ID)) #18
#subset for the unique ones
shelf = shelf[duplicated(shelf$store_product_ID)==FALSE,]

## leave only the necessary columns
shelf <- shelf[,.(storeID, productID, total_shelf_space, mode_shelf_space, mkt_product_store, mkt_subFam_store,
                            mkt_Fam_store, mkt_Grup_store, mkt_Flavor_store, mkt_Type_store, mkt_Units_store)]
master <- readRDS("master_datasplitting.RData")

master <- merge(x = master,y = shelf, by=c("storeID","productID"), all.x=TRUE)

saveRDS(master,"master_mktshare.RData")

