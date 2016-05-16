library(data.table)
library(sampling)

setwd("/media/balint/Storage/Tanulas/thesis/product-variety-optimisation")
# setwd("~/Desktop/BGSE/Term3/MasterProject/GSE")

########################## Split by stores ###################################
# # read in master table
# master <- readRDS("master_table.RData")
# stores <- unique(master[,.(size, storeID)])
# 
# # What do we do with the NA's?????????? Omit them for now.
# stores <- na.omit(stores)
# 
# # sort
# stores <- stores[order(stores$size),]
# table(stores$size)
# 
# # .75*29
# # .75*130
# # .75*79
# # .75*611
# # .75*997
# # .75*658
# # .75*36
# qtys <- c(22, 97, 59, 485, 748, 493, 27)
# 
# s <- strata(stores, "size", qtys, description=TRUE)
# training_stores <- getdata(stores, training_stores)
# #table(training_stores$size)
# training_stores <- training_stores$storeID
# 
# master_train <- master[storeID %in% training_stores]
# master_test <- master[!(storeID %in% training_stores)]
# 
# saveRDS(master_train, file="master_train.RData")
# saveRDS(master_test, file="master_test.RData")




########################## Find Time split ###################################
# Note. This part of the code has not be run as of 3/5/2016
products <- readRDS("products.RData")

# subset products for alltimers
products_alltimers <- products[availability=="alltimer"]$productID

# read in sales
sales <- readRDS("sales.RData")

# find min/max dates across assortment and sales data
min_date_assort <- as.Date("2015-01-01", "%Y-%m-%d")
max_date_assort <- as.Date("2016-03-15", "%Y-%m-%d")
min_date_sales <- min(sales$date)
max_date_sales <- max(sales$date)

min_date <- max(min_date_assort, min_date_sales)
max_date <- min(max_date_assort, max_date_sales)

rm(min_date_sales, min_date_assort, max_date_sales, max_date_assort)

# calculate first and last days for training and test data
total_time <- (max_date-min_date) +1
training_total_days <- round(total_time * 0.75)

training_firstday <- min_date
training_lastday <- min_date + training_total_days -1

test_firstday <- training_lastday +1
test_lastday <- max_date
test_total_days <- (test_lastday - test_firstday) + 1

## Recreate master table, split and aggregate by date

assort_days <- readRDS("product_store_timeline.RData")

assort_train <- assort_days[,1:(3+training_total_days-1), with=FALSE] #in order to have 2015-11-25 as the last day
# NOTE: that when the column count and the date count differ with 1 
# so actually the days are 328 not 329!!!! Because we don't count 2015-03-26 it is our benchmark
assort_test <- cbind(assort_days[,.(storeID, productID, store_product_ID)], 
                     assort_days[,(3+training_total_days):ncol(assort_days), with=FALSE])
rm(assort_days)
#NOTE: the total days in test are 111 b/c we account for the first and the last one

total_days_train <- rowSums(assort_train[,4:ncol(assort_train), with = FALSE])
total_days_test <- rowSums(assort_test[,4:ncol(assort_test), with=FALSE])

assort_train <- as.data.frame(assort_train[,.(storeID,productID)])
assort_train <- as.data.table(cbind(assort_train,total_days_train))
names(assort_train)[3] <- "days_in_assort"

assort_test <- as.data.frame(assort_test[,.(storeID,productID)])
assort_test <- as.data.table(cbind(assort_test,total_days_test))
names(assort_test)[3] <- "days_in_assort"

################################# CHECK ########################################
hist(assort_train$days_in_assort)
hist(assort_test$days_in_assort)

a <- assort_train[days_in_assort==0] # we lose 59,289 observations (if split is 75% / 25%)
b <- assort_test[days_in_assort==0] # we lose 69, 342 observations (if split is 75% / 25%)
################################################################################

# subset sales to match assortment min date
sales <- sales[date >= min_date]
# subset sales to get alltimers
sales<- sales[productID %in% products_alltimers]

######## Training 

#split the sales so it matches training timeline
sales_train= sales[date<=training_lastday]
master_train <- sales_train[,.(total_quantity = sum(quantity_sold_kg)), by=.(productID,storeID)]
## Include 'no. of days in assortment' and 'ave sales per day' columns.  
master_train <- merge(master_train, assort_train, by=c("productID", "storeID"))
master_train[,avg_sales_per_day := total_quantity/days_in_assort]


## Add store variables to master table (note. promo_group not included since it's changing)
stores <- readRDS("stores.RData")
#stores = readRDS("/home/didi/BGSE/semester3/kernel/data/stores.RData")

#removing date and promo_group
stores[,c("date","promo_group") := list(NULL, NULL)]
stores <- unique(stores) #3326
#check if there are any stores in master table that are not in stores table
length(setdiff(master_train$storeID, stores$storeID)) #No
master_train <- merge(master_train, stores, by="storeID")

## Add province and autonomous community data. NB we only have around 2 thirds of the stores here, so we have ~106865 NA's
regions <- readRDS("province.RData")
#regions <- readRDS("/home/didi/BGSE/semester3/kernel/data/province.RData")
regions <- unique(regions) #1781
# find storeID's common to both master table and regions table
stores_used  <- intersect(regions$ipdv, master_train$storeID) #ipdv==storeID
regions      <- regions[ipdv %in% stores_used]
#check that there are now no stores in regions that are not also in master table
length(setdiff(regions$ipdv, master_train$storeID)) #correct
master_train <- merge(master_train, regions, by.x="storeID", by.y="ipdv", all=TRUE)

## Add product variables to master table
products <- readRDS("products.RData")
#products <- readRDS("/home/didi/BGSE/semester3/kernel/data/products.RData")
products[,c("iniDate", "endDate") := list(NULL, NULL)]
products <- unique(products) # doesn't change anything. Already unique.
#check there are no products in master table that are not in products table
length(setdiff(master_train$productID, products$productID)) #No
master_train <- merge(master_train, products, by="productID")

saveRDS(master_train, "master_train.RData")



###### Test 

#split the sales so it matches training timeline
sales_test= sales[date>training_lastday]
master_test <- sales_test[,.(total_quantity = sum(quantity_sold_kg)), by=.(productID,storeID)]
## Include 'no. of days in assortment' and 'ave sales per day' columns.  
master_test <- merge(master_test, assort_test, by=c("productID", "storeID"))
master_test[,avg_sales_per_day := total_quantity/days_in_assort]


## Add store variables to master table (note. promo_group not included since it's changing)
stores <- readRDS("stores.RData")
#stores = readRDS("/home/didi/BGSE/semester3/kernel/data/stores.RData")

#removing date and promo_group
stores[,c("date","promo_group") := list(NULL, NULL)]
stores <- unique(stores) #3326
#check if there are any stores in master table that are not in stores table
length(setdiff(master_test$storeID, stores$storeID)) #No
master_test <- merge(master_test, stores, by="storeID")

## Add province and autonomous community data. NB we only have around 2 thirds of the stores here, so we have ~106865 NA's
regions <- readRDS("province.RData")
#regions <- readRDS("/home/didi/BGSE/semester3/kernel/data/province.RData")
regions <- unique(regions) #1781
# find storeID's common to both master table and regions table
stores_used  <- intersect(regions$ipdv, master_test$storeID) #ipdv==storeID
regions      <- regions[ipdv %in% stores_used]
#check that there are now no stores in regions that are not also in master table
length(setdiff(regions$ipdv, master_test$storeID)) #correct
master_test <- merge(master_test, regions, by.x="storeID", by.y="ipdv", all=TRUE)

## Add product variables to master table
products <- readRDS("products.RData")
#products <- readRDS("/home/didi/BGSE/semester3/kernel/data/products.RData")
products[,c("iniDate", "endDate") := list(NULL, NULL)]
products <- unique(products) # doesn't change anything. Already unique.
#check there are no products in master table that are not in products table
length(setdiff(master_test$productID, products$productID)) #No
master_test <- merge(master_test, products, by="productID")

saveRDS(master_test, "master_test.RData")

####################################################################################################
############################## Check ##############################################################
####################################################################################################
# are there in assortment zero sales 
master_test = readRDS("master_test.RData")
master_train = readRDS("master_train.RData")

nrow(master_test[total_quantity==0])
nrow(master_train[total_quantity==0])

#No such products

# are there zero days in assortment
nrow(master_train[days_in_assort==0])  #32076
nrow(master_test[days_in_assort==0]) #213

#Yes
# we need to clean them, so we take those rows in master tables which are above 5 days 
# in this way we loose around 14% in the training data
# and lee than a percent in the test data 
(nrow(master_train) - nrow(master_train[days_in_assort>5]))/nrow(master_train) 
(nrow(master_test) - nrow(master_test[days_in_assort>5]))/nrow(master_test)

# subset it in to match the assort_day
master_train = master_train[days_in_assort>5]
master_test = master_test[days_in_assort>5]

saveRDS(master_test, "master_test.RData")
saveRDS(master_train, "master_train.RData")