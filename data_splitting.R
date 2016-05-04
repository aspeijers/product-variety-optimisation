library(data.table)
library(sampling)

setwd("~/Desktop/BGSE/Term3/MasterProject/GSE")

########################## Split by stores ###################################
# read in master table
master <- readRDS("master_table.RData")
stores <- unique(master[,.(size, storeID)])

# What do we do with the NA's?????????? Omit them for now.
stores <- na.omit(stores)

# sort
stores <- stores[order(stores$size),]
table(stores$size)

# .75*29
# .75*130
# .75*79
# .75*611
# .75*997
# .75*658
# .75*36
qtys <- c(22, 97, 59, 485, 748, 493, 27)

s <- strata(stores, "size", qtys, description=TRUE)
training_stores <- getdata(stores, training_stores)
#table(training_stores$size)
training_stores <- training_stores$storeID

master_train <- master[storeID %in% training_stores]
master_test <- master[!(storeID %in% training_stores)]

saveRDS(master_train, file="master_train.RData")
saveRDS(master_test, file="master_test.RData")




########################## Find Time split ###################################
# Note. This part of the code has not be run as of 3/5/2016
sales <- readRDS("sales.RData")

# find min/max dates across assortment and sales data
min_date_assort <- as.Date("2015-01-01", "%Y-%m-%d")
max_date_assort <- as.Date("2016-03-15", "%Y-%m-%d")
min_date_sales <- min(sales$date)
max_date_sales <- max(sales$date)

min_date <- max(min_date_assort, min_date_sales)
max_date <- min(max_date_assort, max_date_sales)

# calculate first and last days for training and test data
total_time <- max_date-min_date
training_total_days <- round(total_time * 0.75)

training_firstday <- min_date
training_lastday <- min_date + training_total_days

test_firstday <- training_lastday +1
test_lastday <- max_date

rm(max_date_assort, max_date_sales, min_date_assort, min_date_sales, qtys, total_time)


## Recreate master table, split and aggregate by date
sales <- sales[date > "2014-12-31"]
assort_days <- readRDS("product_store_timeline.RData")

assort_train <- assort_days[,1:(3+training_total_days), with=FALSE]
assort_test <- cbind(assort_days[,.(storeID, productID, store_product_ID)], 
                     assort_days[,(4+training_total_days):ncol(assort_days), with=FALSE])

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


