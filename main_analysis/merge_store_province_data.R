##################################################################################################
######## Description: add the geographical details to the store data  
######## input: stores.RData, province.RData
######## output: stores.RData
##################################################################################################

library(data.table)

#load the two tables 
stores = readRDS("stores.RData")
provinces = readRDS("province.RData")

#merge both of them 
stores = merge(stores,provinces, by.x = "storeID", by.y = "ipdv", all.x = TRUE)

#update names 
names(stores)[8:9] = c("community","province")

#save the file 
saveRDS(stores,"stores.RData")
