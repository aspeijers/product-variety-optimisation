library(data.table)

setwd("~/Desktop/BGSE/Term3/MasterProject/GSE")

## Read in Sales table
# sales<-readRDS("/home/didi/BGSE/semester3/kernel/data/sales.RData")
sales <- readRDS("sales.RData")

# Match the date to the assortment files
sales <- sales[date > "2014-12-31"]


###############################  CHECK  ########################################
# for multiple entries for the same product on the same date and for the same store 
a <- sales[,.(daily_records=length(date)), by=.(storeID,productID)]
b <- sales[,.(uni_daily_records=length(unique(date))), by=.(storeID,productID)]
sum(a$daily_records!=b$uni_daily_records)
# Result - NO
################################################################################


## Read in the assortment table
# assortment_date = readRDS("/home/didi/BGSE/semester3/kernel/data/product_store_timeline_total_days_sales.RData")
assortment_date <- readRDS("product_store_timeline_total_days.RData")
names(assortment_date)[3] <- "days_in_assort"

# aggregate by time and put into the master table
master_table <- sales[,.(total_quantity = sum(quantity_sold_kg)), by=.(productID,storeID)]


###############################  CHECK  ########################################
# if stores and products match in assortment and sales
# stores
ass_store   <- length(unique(assortment_date[,storeID])) # 2682
s_store     <- length(unique(master_table[,storeID])) # 2830
s_store - ass_store  #148
# More stores in sales records - possible since sales data is 'sellIn' (ie from factory to store). We don't have assortment data for all stores. 

# products
ass_product <- length(unique(assortment_date[,productID]))
s_product   <- length(unique(master_table[,productID])) 
s_product - ass_product #-54
# more products in assortment 
################################################################################


## From sales, remove any entries for which we don't have assortment data
master_table <- master_table[storeID %in% assortment_date[,storeID]]
length(unique(master_table[,storeID])) #2666 (< 2682)
# We lose more than 148 entries which means there were also stores in the assortment data which we didn't have in sales.
# Note - we still have more products in assortment 


## Include 'no. of days in assortment' and 'ave sales per day' columns.  
master_table <- merge(master_table, assortment_date, by=c("productID", "storeID"))
master_table[,avg_sales_per_day := total_quantity/days_in_assort]


## Drop any observations where number of days in assortment less than ~5.
master_table <- master_table[days_in_assort > 5]


## Add store variables to master table (note. promo_group not included since it's changing)
stores <- readRDS("stores.RData")
stores[,c("date","promo_group") := list(NULL, NULL)]
stores <- unique(stores) #3326
#check if there are any stores in master table that are not in stores table
length(setdiff(master_table$storeID, stores$storeID)) #No
master_table <- merge(master_table, stores, by="storeID")

## Add province and autonomous community data. NB we only have around 2 thirds of the stores here, so we have ~106865 NA's
regions <- readRDS("province.RData")
regions <- unique(regions) #1781
# find storeID's common to both master table and regions table
stores_used  <- intersect(regions$ipdv, master_table$storeID)
regions      <- regions[ipdv %in% stores_used]
#check that there are now no stores in regions that are not also in master table
length(setdiff(regions$ipdv, master_table$storeID)) #correct
master_table <- merge(master_table, regions, by.x="storeID", by.y="ipdv", all=TRUE)

## Add product variables to master table
products <- readRDS("products.RData")
products[,c("iniDate", "endDate") := list(NULL, NULL)]
products <- unique(products) # doesn't change anything. Already unique.
#check there are no products in master table that are not in products table
length(setdiff(master_table$productID, products$productID)) #No
master_table <- merge(master_table, products, by="productID")


###############################  CHECK  ########################################
# how many entries we have NA's for 
a <- master_table[is.na(town)] #82,736 entries with town=NA

b <- master_table[is.na(chain)] #14,024 entries with chain=NA
table(b$sub_chain) # relate to subchains 2224 and 3154
bb <- stores[sub_chain %in% c(2224, 3154)] 
summary(bb$chain) # no infomation on chain available for these subchains

# how many entries do we lose?
c <- master_table[!is.na(town)]
c <- c[!is.na(chain)] 
dim(c)[1] #287,867 entries

# we have lost:
dim(master_table)[1] - dim(c)[1] #91,739 entries lost

# leave them in there for the moment, in case we don't use these variables.
################################################################################

# replace empty cells in size column with NA.
is.na(master_table$size) <- which(master_table$size == "")

# save master table
saveRDS(master_table, file="master_table.RData")



