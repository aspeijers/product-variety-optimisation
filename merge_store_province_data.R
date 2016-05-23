library(data.table)
setwd("~/BGSE/semester3/kernel/data")

#load the two tables 
stores = readRDS("stores.RData")
provinces = readRDS("province.RData")

#merge both of them 
stores = merge(stores,provinces, by.x = "storeID", by.y = "ipdv", all.x = TRUE)

#update names 
names(stores)[8:9] = c("community","province")

#save the file 
saveRDS(stores,"stores.RData")
