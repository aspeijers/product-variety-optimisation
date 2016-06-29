##################################################################################################
######## Description: adding flavour and type to the products (possibly to the master table as well)  
######## input: skuFlavour.RData, products.RData, master_mktshare.RData
######## output: products.RData, master_mktshare.RData
##################################################################################################


### Libraries
library(data.table)

### Tables
flavor_data<-readRDS("skuFlavor.RData")
#flavor_data<-readRDS("old/skuFlavor.RData")

#rename the column
names(flavor_data)[1]<-"productID"
#saveRDS(flavor_data,"flavor_dat.RData")

# deal with the new products table
products <- readRDS("skuClean2.RData")
#products <- readRDS("old/skuClean2.RData")
names(products)[1]<- c("productID")


# combine both of them 
products <- merge(products, flavor_data, by.x="productID", by.y="productID", all=TRUE)

# change the units of measure in the weight column
products[,weights_kg := weight/1000]
products[,weight := NULL]


##############################################################################
######### Add availability column
########  NOTE: This part maybe redundant (depends on the sequence of 
########        implementing the R files)
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
#saveRDS(products, file= "products.RData")

#remove unnecessary tables
rm(flavor_data,prod,product_timeline, sales, productIDs)

# read in master table (after creation of market share variables)
master <- readRDS("master_mktshare.RData")



# merge in flavour, type and weights_kg
products <- products[,.(productID, flavor, type, weights_kg)]
master <- merge(master_train, products, by="productID", all.x=TRUE)


saveRDS(master, "master_mktshare.RData")










