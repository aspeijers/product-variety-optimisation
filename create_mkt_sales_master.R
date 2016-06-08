#setwd("~/Desktop/BGSE/Term3/MasterProject/GSE")

library(data.table)

master <- readRDS("master_mktshare.RData")



########### Mkt share of product within each store (by sales)
# create average sales per day per store
master_bystore <- master[,.(storeID, total_quantity, days_in_assort)]
master_bystore <- master_bystore[,lapply(.SD, sum), by=storeID]
master_bystore <- master_bystore[,avg_sales_store:= total_quantity/days_in_assort]

# merge this back into the master table
master_bystore <- master_bystore[,.(storeID, avg_sales_store)]
master <- merge(master, master_bystore, by="storeID", all.x = TRUE)
master<- master[,mkt_product_store_sales:=avg_sales_per_day/avg_sales_store]

rm(master_bystore)

########### Mkt share of subFam within each store (by sales)
# create average sales per day per store
master_bysubFam <- master[,.(storeID, subFam, total_quantity, days_in_assort)]
master_bysubFam <- master_bysubFam[,lapply(.SD, sum), by=.(storeID, subFam)]
master_bysubFam <- master_bysubFam[,avg_sales_subFam:= total_quantity/days_in_assort]

# merge this back into the master table
master_bysubFam <- master_bysubFam[,.(storeID, subFam, avg_sales_subFam)]
master <- merge(master, master_bysubFam, by=c("storeID","subFam"), all.x = TRUE)
master<- master[,mkt_subFam_store_sales:=avg_sales_subFam/avg_sales_store]

rm(master_bysubFam)

########### Mkt share of Fam within each store (by sales)
# create average sales per day per store
master_byFam <- master[,.(storeID, fam, total_quantity, days_in_assort)]
master_byFam <- master_byFam[,lapply(.SD, sum), by=.(storeID, fam)]
master_byFam <- master_byFam[,avg_sales_Fam:= total_quantity/days_in_assort]

# merge this back into the master table
master_byFam <- master_byFam[,.(storeID, fam, avg_sales_Fam)]
master <- merge(master, master_byFam, by=c("storeID","fam"), all.x = TRUE)
master<- master[,mkt_Fam_store_sales:=avg_sales_Fam/avg_sales_store]

rm(master_byFam)

########### Mkt share of Grup within each store (by sales)
# create average sales per day per store
master_byGrup <- master[,.(storeID, grup, total_quantity, days_in_assort)]
master_byGrup <- master_byGrup[,lapply(.SD, sum), by=.(storeID, grup)]
master_byGrup <- master_byGrup[,avg_sales_Grup:= total_quantity/days_in_assort]

# merge this back into the master table
master_byGrup <- master_byGrup[,.(storeID, grup, avg_sales_Grup)]
master <- merge(master, master_byGrup, by=c("storeID","grup"), all.x = TRUE)
master<- master[,mkt_Grup_store_sales:=avg_sales_Grup/avg_sales_store]

rm(master_byGrup)

########### Mkt share of Flavor within each store (by sales)
# create average sales per day per store
master_byFlavor <- master[,.(storeID, flavor, total_quantity, days_in_assort)]
master_byFlavor <- master_byFlavor[,lapply(.SD, sum), by=.(storeID, flavor)]
master_byFlavor <- master_byFlavor[,avg_sales_Flavor:= total_quantity/days_in_assort]

# merge this back into the master table
master_byFlavor <- master_byFlavor[,.(storeID, flavor, avg_sales_Flavor)]
master <- merge(master, master_byFlavor, by=c("storeID","flavor"), all.x = TRUE)
master<- master[,mkt_Flavor_store_sales:=avg_sales_Flavor/avg_sales_store]

rm(master_byFlavor)

########### Mkt share of Type within each store (by sales)
# create average sales per day per store
master_byType <- master[,.(storeID, type, total_quantity, days_in_assort)]
master_byType <- master_byType[,lapply(.SD, sum), by=.(storeID, type)]
master_byType <- master_byType[,avg_sales_Type:= total_quantity/days_in_assort]

# merge this back into the master table
master_byType <- master_byType[,.(storeID, type, avg_sales_Type)]
master <- merge(master, master_byType, by=c("storeID","type"), all.x = TRUE)
master<- master[,mkt_Type_store_sales:=avg_sales_Type/avg_sales_store]

rm(master_byType)

########### Mkt share of Units within each store (by sales)
# create average sales per day per store
master_byUnits <- master[,.(storeID, units, total_quantity, days_in_assort)]
master_byUnits <- master_byUnits[,lapply(.SD, sum), by=.(storeID, units)]
master_byUnits <- master_byUnits[,avg_sales_Units:= total_quantity/days_in_assort]

# merge this back into the master table
master_byUnits <- master_byUnits[,.(storeID, units, avg_sales_Units)]
master <- merge(master, master_byUnits, by=c("storeID","units"), all.x = TRUE)
master<- master[,mkt_Units_store_sales:=avg_sales_Units/avg_sales_store]

rm(master_byUnits)

########### remove unnecessary columns
master[,c("avg_sales_store", "avg_sales_subFam", "avg_sales_Fam", 
               "avg_sales_Grup", "avg_sales_Flavor", "avg_sales_Type", "avg_sales_Units"):=NULL]


########### Create standardised total quantity per store variable
master_bystore <- master[,.(storeID, total_quantity)]
master_bystore <- master_bystore[,lapply(.SD, sum), by=storeID]
master_bystore <- master_bystore[,total_quantity:= scale(total_quantity)]
names(master_bystore)[2] <- "store_total_quantity"

# merge this back into the master table
master <- merge(master, master_bystore, by="storeID", all.x = TRUE)


########### write file
saveRDS(master, "master_mktshare.RData")


