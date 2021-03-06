##################################################################################################
######## Description: Renaming the Tables (additional infro in data_info.ods)   
######## input: selInClean.RData, skuClean2.RData,pdvClean.RData, assortment folder
######## output: sales.RData, products.RData, stores,RData, assortment folder
##################################################################################################


library(data.table)

# Sales Table 
sales <- readRDS("selInClean.RData")
names(sales)<-c("productID","storeID","quantity_sold_kg","date")
saveRDS(sales,"sales.RData")

# Product Table
products <- readRDS("skuClean2.RData")
names(products)[1]<- c("productID")
saveRDS(products, "products.RData")

# Stores Table
stores <- readRDS("pdvClean.RData")
#remove client 50
stores <- stores[,"client50":=NULL]
names(stores)<-c("storeID","promo_group","sub_chain", "chain", "town", "size", "date")
saveRDS(stores, "stores.RData")

# Variety 
path = "/assortment/"

file.names <- dir(path, pattern =".RData")
for(i in 1:length(file.names)){
  file <- readRDS(file.names[i])
  # rename the columns 
  names(file)<-c("storeID","productID","shelf_space")
  # extract the date from the file name 
  new.name = regmatches(file.names[i], regexpr("[0-9].*[0-9]", file.names[i]))
  #write extension RData to the file
  new.name.extension = paste(new.name,"RData", sep=".")
  # save the new file
  saveRDS(file,new.name.extension)
}


