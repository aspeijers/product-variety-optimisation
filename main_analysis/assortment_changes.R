##################################################################################################
######## Description: Finding alltimer products and tagging "not_in_assortment_data"   
######## input: product_store_timeline.RData, sales.RData, product.RData
######## output: assortment_changes.RData, availability column in the product.RData; product_timeline.RData
################################################################################################## 

library(data.table)

#creating aggregation by product
library(reshape2)
product_store_timeline <- readRDS("product_store_timeline.RData")

#first half
#deleting the productIDstoreID variable
product_store_timeline <- product_store_timeline[1:nrow(product_store_timeline), -3, with=FALSE]
#the long version of the data is too big for memory, so we do the aggregation in two parts
product_store_timeline1 <- product_store_timeline[1:200000,]
rm(product_store_timeline)
product_store_timeline1_long <- melt(product_store_timeline1, id.vars = c("storeID", "productID"))
rm(product_store_timeline1)
product_timeline1 <- product_store_timeline1_long[,.(market_size = sum(value)), by=.(productID, variable)]
rm(product_store_timeline1_long)

#second half
product_store_timeline <- readRDS("product_store_timeline.RData")
product_store_timeline <- product_store_timeline[1:nrow(product_store_timeline), -3, with=FALSE]
product_store_timeline2 <- product_store_timeline[200001:nrow(product_store_timeline),]
rm(product_store_timeline)
product_store_timeline2_long <- melt(product_store_timeline2, id.vars = c("storeID", "productID"))
rm(product_store_timeline2)
product_timeline2 <- product_store_timeline2_long[,.(market_size = sum(value)), by=.(productID, variable)]
rm(product_store_timeline2_long)

#combining the two halves
product_timeline <- rbind(product_timeline1, product_timeline2)
#turning into dates
product_timeline$variable <- as.Date(product_timeline$variable, '%Y%m%d')
names(product_timeline) <- c("productID", "date", "market_size")
product_timeline <- product_timeline[,.(market_size = sum(market_size)), by=.(productID, date)]
saveRDS(product_timeline, file= "product_timeline.RData")

product_timeline <- readRDS("product_timeline.RData")


#UPDATING THE PRODUCTS TABLE
products <- readRDS("products.RData")
sales <- readRDS("sales.RData")
#take only the products which are in sales
products <- products[products$productID %in% sales$productID,]
#summary information about products
prod <- product_timeline[,.(min = min(market_size), avg = mean(market_size),
                            max = max(market_size), firstweek = sum(head(market_size, 7)),
                            lastweek = sum(tail(market_size,7))), by=.(productID)]

#introducing availability variable,
#a factor telling about the time structure of the availability of the product
products$availability <- NA
#alltimers are products which were available in more than 500 stores during the whole period
products$availability[products$productID %in% prod[prod$min>500,]$productID] <- "alltimer"
productIDs <- unique(product_timeline$productID)
products$availability[!(products$productID %in% productIDs)] <- "not_in_assortment_data"
saveRDS(products, file= "products.RData")



# p<- prod[prod$productID %in% notalltimer,]
# notalltimer<- prod[prod$min<500,]$productID
# library(ggplot2)
# for(i in 1:15){
#     from <- 10*i-9
#     until <- 10*i
#     p<- product_timeline[product_timeline$productID %in% notalltimer[from:until],]
#     ggp <- ggplot(p, aes(x=date, y=market_size, colour=productID)) +
#         geom_line() +
#         theme_bw()
#     print(ggp)
# }
# ggplot(product_timeline[product_timeline$productID==productIDs[3],], aes(x=date, y=market_size)) +
#     geom_line() +
#     theme_bw()


#CREATING AND SAVING ASSORTMENT_CHANGE TABLE,
#which is 1 when a product is introduced to a store,
#-1 when taken out, and 0 otherwise
product_store_timeline <- as.data.frame(readRDS("product_store_timeline.RData"))
assortment_change <- product_store_timeline[, -4]
for(i in 4:440){
    #cat(i)
    assortment_change[,i] <- product_store_timeline[,i+1]-product_store_timeline[,i]
}
saveRDS(assortment_change, file="assortment_change.RData")