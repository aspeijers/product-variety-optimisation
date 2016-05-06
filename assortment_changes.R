setwd("/media/balint/Storage/Tanulas/thesis/product-variety-optimisation")
library(data.table)
#deleting the productIstoreID variable
# product_store_timeline <- readRDS("product_store_timeline.RData")
# product_store_timeline <- product_store_timeline[1:394582, -3, with=FALSE]
# saveRDS(product_store_timeline,file = "/media/balint/Storage/Tanulas/thesis/product-variety-optimisation/product_store_timeline.RData")

#creating aggregation by product
library(reshape2)
product_store_timeline <- readRDS("product_store_timeline.RData")
#the long version of the data is too big for memory, so we do the aggregation in two parts
product_store_timeline1 <- product_store_timeline[1:200057,]
rm(product_store_timeline)
product_store_timeline1_long <- melt(product_store_timeline1, id.vars = c("storeID", "productID"))
product_timeline1 <- product_store_timeline1_long[,.(market_size = sum(value)), by=.(productID, variable)]
rm(product_store_timeline1_long)
product_store_timeline <- readRDS("product_store_timeline.RData")
product_store_timeline2 <- product_store_timeline[200058:394582,]
rm(product_store_timeline)
product_store_timeline2_long <- melt(product_store_timeline2, id.vars = c("storeID", "productID"))
rm(product_store_timeline2)
product_timeline2 <- product_store_timeline2_long[,.(market_size = sum(value)), by=.(productID, variable)]
rm(product_store_timeline2_long)
product_timeline <- rbind(product_timeline1, product_timeline2)
product_timeline <- product_timeline[,.(market_size = sum(market_size)), by=.(productID, date)]
product_timeline <- data.frame(product_timeline)

#turning into dates
product_timeline$variable <- as.Date(product_timeline$variable, '%Y%m%d')
names(product_timeline) <- c("productID", "date", "market_size")
saveRDS(product_timeline, file= "product_timeline.RData")
#test
productIDs <- unique(product_timeline$productID)

library(ggplot2)
for(i in 1:31){
    from <- 10*i-9
    until <- 10*i
    p<- product_timeline[product_timeline$productID %in% productIDs[from:until],]
    ggp <- ggplot(p, aes(x=date, y=market_size, colour=productID)) +
        geom_line() +
        theme_bw()
    print(ggp)
}
ggplot(product_timeline[product_timeline$productID=="G19F03S01S04",], aes(x=date, y=market_size)) +
    geom_line() +
    theme_bw()


assortment_change <- product_store_timeline[, -4]

for(i in 4:440){
    assortment_change[,i] <- product_store_timeline[,i+1]-product_store_timeline[,i]
}
saveRDS(assortment_change, file="assortment_change.RData")


table(assortment_change[,45])

sum(rowSums(assortment_change[,4:440]))
assortment_change[,1:3] <- product_store_timeline[,1:3]
product_store_timeline[, `:=`(c20150102 =  20150101!=20150102,
                              c20150103 =  20150102!=20150103)]

# a <- product_store_timeline[, 4, with = FALSE]
# b <- product_store_timeline[,5, with = FALSE]
# ab <- a!=b
# assortment_change[,storeID] := product_store_timeline[,storeID]
# assortment_change[,1:3] <- product_store_timeline[,1:3, with=FALSE]
# c <- data.table(names(assortment_change))
# s<-product_store_timeline[,storeID]
# assortment_change[, storeID = s]
# assortment_change[,1:3]