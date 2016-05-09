setwd("/media/balint/Storage/Tanulas/thesis/product-variety-optimisation")
library(data.table)

#CREATING sales_by_month
sales <- readRDS("sales.RData")
sales$month <- strftime(sales$date,format="%Y-%m")
sales_by_month <- sales[,.(quantity_sold_kg = sum(quantity_sold_kg)), by=.(productID, storeID, month)]
rm(sales)
sales_by_month$month <- as.Date(paste(sales_by_month$month,1),"%Y-%m %d")
saveRDS(sales_by_month, "sales_by_month.RData")


#VISUALIZATION
sales_by_month <- readRDS("sales_by_month.RData")
library(ggplot2)

plotsales <- function(pID, sID){
    ggplot(sales_by_month[sales_by_month$productID==pID & sales_by_month$storeID==sID,], aes(x=month, y=quantity_sold_kg)) +
        geom_line()
}


#TEST
# pID = sales_by_month$productID[200066]
# sID = sales_by_month$storeID[500000]
# plotsales(pID, sID)