library(data.table)

#setwd("/media/balint/Storage/Tanulas/thesis/product-variety-optimisation")
#setwd("~/Desktop/BGSE/Term3/MasterProject/GSE")

########################## Find Time split ###################################
products <- readRDS("products.RData")

# subset products for alltimers
products_alltimers <- products[availability=="alltimer"]$productID

# read in sales
sales <- readRDS("sales.RData")

## Recreate master table, split and aggregate by date
assort_days <- readRDS("product_store_timeline.RData")

#################################### CHECK #####################################
# check how many unique productID, storeID tuples we have 
train_dup <- duplicated(assort_days[,.(productID, storeID)])
sum(train_dup) # 0 indicates no duplicates

rm(train_dup)
################################################################################

# create total days in assortment variable
total_days <- rowSums(assort_days[,4:ncol(assort_days), with = FALSE])


#assort_ <- as.data.frame(assort_days[,.(storeID,productID)])
assort_days <-cbind(assort_days[,.(storeID,productID)],total_days)
names(assort_days)[3] <- "days_in_assort"

rm(total_days)

################################# CHECK ########################################
a <- assort_days[days_in_assort==0] # None 
################################################################################

# subset sales to get alltimers
sales <- sales[productID %in% products_alltimers]


# sum sales by date
master<- sales[,.(total_quantity = sum(quantity_sold_kg)), by=.(productID,storeID)]
## Include 'no. of days in assortment'
master <- merge(master, assort_days, by=c("productID", "storeID"))
# remove rows which have days_in_assort = 0
master= master[days_in_assort != 0]
# calculate avg_sales_per_day
master[,avg_sales_per_day := total_quantity/days_in_assort]


## Add store variables to master table (note. promo_group not included since it's changing)
stores <- readRDS("stores.RData")
#stores = readRDS("/home/didi/BGSE/semester3/kernel/data/stores.RData")

#removing date and promo_group
stores[,c("date","promo_group") := list(NULL, NULL)]
stores <- unique(stores) #3326
#check if there are any stores in master table that are not in stores table
#length(setdiff(master_train$storeID, stores$storeID)) # 0 indicates no

# add store variables to master_train
master<- merge(master, stores, by="storeID", all.x = T)


## Add product variables to master table
products[,c("iniDate", "endDate") := list(NULL, NULL)]
master<- merge(master, products, by="productID")

## Filter train and test tables for days_in_assort > 5
nrow(master[total_quantity==0])
#No such products


# subset it in to match the assort_day
master = master[days_in_assort>5]


saveRDS(master, "master_datasplitting.RData")


