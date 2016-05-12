### Libraries
library(data.table)
library(stringr)

setwd("~/BGSE/semseter3/kernel/data")

### Tables
flavor_data<-readRDS("skuFlavor.RData")
#rename the column
names(flavor_data)[1]<-"productID"
#saveRDS(flavor_data,"flavor_dat.RData")

products = readRDS("products.RData") #I have renamed the new skuClean2.RData file

# combine both of them 
products <- merge(products, flavor_data, by.x="productID", by.y="productID", all=TRUE)
saveRDS(products,"products.RData")






