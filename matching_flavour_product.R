### Libraries
library(data.table)

setwd("~/BGSE/semester3/kernel/data")

### Tables
flavor_data<-readRDS("skuFlavor.RData")
#rename the column
names(flavor_data)[1]<-"productID"
#saveRDS(flavor_data,"flavor_dat.RData")

# deal with the new products table
products <- readRDS("skuClean2.RData")
names(products)[1]<- c("productID")


# combine both of them 
products <- merge(products, flavor_data, by.x="productID", by.y="productID", all=TRUE)

# change the measure units in the weight column
products[,weights_kg := weight/1000]
products[,weight := NULL]

# add the column for availability 

###############################################################################
# if you haven't overwritten the product data then this bit is not necessary
# you just need to merge the old products with the created one here
products_old <- readRDS("products.RData")
products_old[order(productID)]
products[order(productID)]
products[,availability := products_old$availability]

saveRDS(products, file= "products.RData")

##############################################################################
######### If not run this bit
#############################################################################
#UPDATING THE PRODUCTS TABLE
product_timeline <- readRDS("product_timeline.RData")
sales <- readRDS("sales.RData")

#take only the products which are in sales
products <- products[products$productID %in% sales$productID,]

#summary information about products
prod <- product_timeline[,.(min = min(market_size)), by=.(productID)]

#introducing availability variable,
#a factor telling about the time structure of the availability of the product
products$availability <- NA
#alltimers are products which were available in more than 500 stores during the whole period
products$availability[products$productID %in% prod[prod$min>500,]$productID] <- "alltimer"
productIDs <- unique(product_timeline$productID)
products$availability[!(products$productID %in% productIDs)] <- "not_in_assortment_data"

#Save the results
saveRDS(products, file= "products.RData")












