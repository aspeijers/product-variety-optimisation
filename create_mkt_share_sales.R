setwd("~/Desktop/BGSE/Term3/MasterProject/GSE")

library(data.table)

############################### TRAIN ##########################################
master_train <- readRDS("master_train_mktshare.RData")
sales_train <- as.data.table(as.data.frame(master_train))
rm(master_train)

#sales_train[, c("avg_sales_store","mkt_product_store_sales","avg_sales_subFam","mkt_subFam_store_sales","avg_sales_Fam","mkt_Fam_store_sales","avg_sales_Grup","mkt_Grup_store_sales"):=NULL]

########### Mkt share of product within each store (by sales)
# create average sales per day per store
sales_train_bystore <- sales_train[,.(storeID, total_quantity, days_in_assort)]
sales_train_bystore <- sales_train_bystore[,lapply(.SD, sum), by=storeID]
sales_train_bystore <- sales_train_bystore[,avg_sales_store:= total_quantity/days_in_assort]

# merge this back into the sales_train table
sales_train_bystore <- sales_train_bystore[,.(storeID, avg_sales_store)]
sales_train <- merge(sales_train, sales_train_bystore, by="storeID", all.x = TRUE)
sales_train<- sales_train[,mkt_product_store_sales:=avg_sales_per_day/avg_sales_store]

rm(sales_train_bystore)

########### Mkt share of subFam within each store (by sales)
# create average sales per day per store
sales_train_bysubFam <- sales_train[,.(storeID, subFam, total_quantity, days_in_assort)]
sales_train_bysubFam <- sales_train_bysubFam[,lapply(.SD, sum), by=.(storeID, subFam)]
sales_train_bysubFam <- sales_train_bysubFam[,avg_sales_subFam:= total_quantity/days_in_assort]

# merge this back into the sales_train table
sales_train_bysubFam <- sales_train_bysubFam[,.(storeID, subFam, avg_sales_subFam)]
sales_train <- merge(sales_train, sales_train_bysubFam, by=c("storeID","subFam"), all.x = TRUE)
sales_train<- sales_train[,mkt_subFam_store_sales:=avg_sales_subFam/avg_sales_store]

rm(sales_train_bysubFam)

########### Mkt share of Fam within each store (by sales)
# create average sales per day per store
sales_train_byFam <- sales_train[,.(storeID, fam, total_quantity, days_in_assort)]
sales_train_byFam <- sales_train_byFam[,lapply(.SD, sum), by=.(storeID, fam)]
sales_train_byFam <- sales_train_byFam[,avg_sales_Fam:= total_quantity/days_in_assort]

# merge this back into the sales_train table
sales_train_byFam <- sales_train_byFam[,.(storeID, fam, avg_sales_Fam)]
sales_train <- merge(sales_train, sales_train_byFam, by=c("storeID","fam"), all.x = TRUE)
sales_train<- sales_train[,mkt_Fam_store_sales:=avg_sales_Fam/avg_sales_store]

rm(sales_train_byFam)

########### Mkt share of Grup within each store (by sales)
# create average sales per day per store
sales_train_byGrup <- sales_train[,.(storeID, grup, total_quantity, days_in_assort)]
sales_train_byGrup <- sales_train_byGrup[,lapply(.SD, sum), by=.(storeID, grup)]
sales_train_byGrup <- sales_train_byGrup[,avg_sales_Grup:= total_quantity/days_in_assort]

# merge this back into the sales_train table
sales_train_byGrup <- sales_train_byGrup[,.(storeID, grup, avg_sales_Grup)]
sales_train <- merge(sales_train, sales_train_byGrup, by=c("storeID","grup"), all.x = TRUE)
sales_train<- sales_train[,mkt_Grup_store_sales:=avg_sales_Grup/avg_sales_store]

rm(sales_train_byGrup)

########### Mkt share of Flavor within each store (by sales)
# create average sales per day per store
sales_train_byFlavor <- sales_train[,.(storeID, flavor, total_quantity, days_in_assort)]
sales_train_byFlavor <- sales_train_byFlavor[,lapply(.SD, sum), by=.(storeID, flavor)]
sales_train_byFlavor <- sales_train_byFlavor[,avg_sales_Flavor:= total_quantity/days_in_assort]

# merge this back into the sales_train table
sales_train_byFlavor <- sales_train_byFlavor[,.(storeID, flavor, avg_sales_Flavor)]
sales_train <- merge(sales_train, sales_train_byFlavor, by=c("storeID","flavor"), all.x = TRUE)
sales_train<- sales_train[,mkt_Flavor_store_sales:=avg_sales_Flavor/avg_sales_store]

rm(sales_train_byFlavor)

########### Mkt share of Type within each store (by sales)
# create average sales per day per store
sales_train_byType <- sales_train[,.(storeID, type, total_quantity, days_in_assort)]
sales_train_byType <- sales_train_byType[,lapply(.SD, sum), by=.(storeID, type)]
sales_train_byType <- sales_train_byType[,avg_sales_Type:= total_quantity/days_in_assort]

# merge this back into the sales_train table
sales_train_byType <- sales_train_byType[,.(storeID, type, avg_sales_Type)]
sales_train <- merge(sales_train, sales_train_byType, by=c("storeID","type"), all.x = TRUE)
sales_train<- sales_train[,mkt_Type_store_sales:=avg_sales_Type/avg_sales_store]

rm(sales_train_byType)

########### remove unnecessary columns
sales_train[,c("avg_sales_store", "avg_sales_subFam", "avg_sales_Fam", "avg_sales_Grup", "avg_sales_Flavor", "avg_sales_Type"):=NULL]

########### write file
saveRDS(sales_train, "master_train_mktshare.RData")

################################ TEST ##########################################
master_test <- readRDS("master_test_mktshare.RData")
sales_test <- as.data.table(as.data.frame(master_test))
rm(master_test)

#sales_test[, c("mkt_product_store_sales","mkt_subFam_store_sales","mkt_Fam_store_sales","mkt_Grup_store_sales"):=NULL]

########### Mkt share of product within each store (by sales)
# create average sales per day per store
sales_test_bystore <- sales_test[,.(storeID, total_quantity, days_in_assort)]
sales_test_bystore <- sales_test_bystore[,lapply(.SD, sum), by=storeID]
sales_test_bystore <- sales_test_bystore[,avg_sales_store:= total_quantity/days_in_assort]

# merge this back into the sales_test table
sales_test_bystore <- sales_test_bystore[,.(storeID, avg_sales_store)]
sales_test <- merge(sales_test, sales_test_bystore, by="storeID", all.x = TRUE)
sales_test<- sales_test[,mkt_product_store_sales:=avg_sales_per_day/avg_sales_store]

rm(sales_test_bystore)

########### Mkt share of subFam within each store (by sales)
# create average sales per day per store
sales_test_bysubFam <- sales_test[,.(storeID, subFam, total_quantity, days_in_assort)]
sales_test_bysubFam <- sales_test_bysubFam[,lapply(.SD, sum), by=.(storeID, subFam)]
sales_test_bysubFam <- sales_test_bysubFam[,avg_sales_subFam:= total_quantity/days_in_assort]

# merge this back into the sales_test table
sales_test_bysubFam <- sales_test_bysubFam[,.(storeID, subFam, avg_sales_subFam)]
sales_test <- merge(sales_test, sales_test_bysubFam, by=c("storeID","subFam"), all.x = TRUE)
sales_test<- sales_test[,mkt_subFam_store_sales:=avg_sales_subFam/avg_sales_store]

rm(sales_test_bysubFam)

########### Mkt share of Fam within each store (by sales)
# create average sales per day per store
sales_test_byFam <- sales_test[,.(storeID, fam, total_quantity, days_in_assort)]
sales_test_byFam <- sales_test_byFam[,lapply(.SD, sum), by=.(storeID, fam)]
sales_test_byFam <- sales_test_byFam[,avg_sales_Fam:= total_quantity/days_in_assort]

# merge this back into the sales_test table
sales_test_byFam <- sales_test_byFam[,.(storeID, fam, avg_sales_Fam)]
sales_test <- merge(sales_test, sales_test_byFam, by=c("storeID","fam"), all.x = TRUE)
sales_test<- sales_test[,mkt_Fam_store_sales:=avg_sales_Fam/avg_sales_store]

rm(sales_test_byFam)

########### Mkt share of Grup within each store (by sales)
# create average sales per day per store
sales_test_byGrup <- sales_test[,.(storeID, grup, total_quantity, days_in_assort)]
sales_test_byGrup <- sales_test_byGrup[,lapply(.SD, sum), by=.(storeID, grup)]
sales_test_byGrup <- sales_test_byGrup[,avg_sales_Grup:= total_quantity/days_in_assort]

# merge this back into the sales_test table
sales_test_byGrup <- sales_test_byGrup[,.(storeID, grup, avg_sales_Grup)]
sales_test <- merge(sales_test, sales_test_byGrup, by=c("storeID","grup"), all.x = TRUE)
sales_test<- sales_test[,mkt_Grup_store_sales:=avg_sales_Grup/avg_sales_store]

rm(sales_test_byGrup)

########### Mkt share of Flavor within each store (by sales)
# create average sales per day per store
sales_test_byFlavor <- sales_test[,.(storeID, flavor, total_quantity, days_in_assort)]
sales_test_byFlavor <- sales_test_byFlavor[,lapply(.SD, sum), by=.(storeID, flavor)]
sales_test_byFlavor <- sales_test_byFlavor[,avg_sales_Flavor:= total_quantity/days_in_assort]

# merge this back into the sales_test table
sales_test_byFlavor <- sales_test_byFlavor[,.(storeID, flavor, avg_sales_Flavor)]
sales_test <- merge(sales_test, sales_test_byFlavor, by=c("storeID","flavor"), all.x = TRUE)
sales_test<- sales_test[,mkt_Flavor_store_sales:=avg_sales_Flavor/avg_sales_store]

rm(sales_test_byFlavor)

########### Mkt share of Type within each store (by sales)
# create average sales per day per store
sales_test_byType <- sales_test[,.(storeID, type, total_quantity, days_in_assort)]
sales_test_byType <- sales_test_byType[,lapply(.SD, sum), by=.(storeID, type)]
sales_test_byType <- sales_test_byType[,avg_sales_Type:= total_quantity/days_in_assort]

# merge this back into the sales_test table
sales_test_byType <- sales_test_byType[,.(storeID, type, avg_sales_Type)]
sales_test <- merge(sales_test, sales_test_byType, by=c("storeID","type"), all.x = TRUE)
sales_test<- sales_test[,mkt_Type_store_sales:=avg_sales_Type/avg_sales_store]

rm(sales_test_byType)

########### remove unnecessary columns
sales_test[,c("avg_sales_store", "avg_sales_subFam", "avg_sales_Fam", "avg_sales_Grup", "avg_sales_Flavor", "avg_sales_Type"):=NULL]

########### write file
saveRDS(sales_test, "master_test_mktshare.RData")
