setwd("/media/balint/Storage/Tanulas/thesis/product-variety-optimisation")
library(data.table)

#CREATING sales_by_month
#this part takes a few minutes to run
sales <- readRDS("sales.RData")
sales$month <- strftime(sales$date,format="%Y-%m")
sales_by_month <- sales[,.(quantity_sold_kg = sum(quantity_sold_kg)), by=.(productID, storeID, month)]
rm(sales)
sales_by_month$month <- as.Date(paste(sales_by_month$month,1),"%Y-%m %d")
sales_by_month$productID <- as.factor(sales_by_month$productID)
sales_by_month$storeID <- as.factor(sales_by_month$storeID)

#join with stores and products data
products <- readRDS("products.RData")
stores <- readRDS("stores.RData")
stores[,c("date","promo_group") := list(NULL, NULL)]
stores <- unique(stores)
stores$storeID <- as.factor(stores$storeID)
sales_by_month <- merge(products, sales_by_month, by = "productID")
sales_by_month <- merge(stores, sales_by_month, by = "storeID")

saveRDS(sales_by_month, "sales_by_month.RData")


#VISUALIZATION
sales_by_month <- readRDS("sales_by_month.RData")
library(ggplot2)
library(plyr)

#this function is for plotting sales as time series
#sales_by_month is the data frame needed
#id_pairs id the data frame with productID and storeID pairs
#plot_by is the factor variable by which you want to split the data
plotsales <- function(id_pairs, plot_by, sales_by_month){
    datatoplot <- merge(id_pairs, sales_by_month, by = c("storeID", "productID"))
    datatoplot <- ddply(datatoplot, .(get(plot_by), month), summarize, quantity_sold_kg = sum(quantity_sold_kg))
    names(datatoplot)[1] <- plot_by
    ggplot(datatoplot, aes_string(x="month", y="quantity_sold_kg", group = plot_by, colour = plot_by)) +
        geom_line() +
        theme_bw()
}
#TEST

# master_train <- readRDS("master_train.RData")
# master_train$productID <- as.factor(master_train$productID)
# master_train$storeID <- as.factor(master_train$storeID)
# t <- head(master_train[, .(storeID, productID)])
# plotsales(t, plot_by = "storeID", sales_by_month)
# plotsales(master_train[, .(storeID, productID)], plot_by = "community", sales_by_month)


#stacked barplot
library(ggplot2)
library(data.table)
master_table <- readRDS("master_table.RData")
master_table$chain <- as.factor(master_table$chain)
master_table$sub_chain <- as.factor(master_table$sub_chain)

ggplot(master_table, aes(x = chain, y = total_quantity, fill=sub_chain, group=sub_chain)) + 
    geom_bar(stat = "summary", fun.y=sum)#this function is for plotting sales as time series
#sales_by_month is the data frame needed
#id_pairs id the data frame with productID and storeID pairs
#plot_by is the factor variable by which you want to split the data
plotsales <- function(id_pairs, plot_by, sales_by_month){
    datatoplot <- merge(id_pairs, sales_by_month, by = c("storeID", "productID"))
    datatoplot <- ddply(datatoplot, .(get(plot_by), month), summarize, quantity_sold_kg = sum(quantity_sold_kg))
    names(datatoplot)[1] <- plot_by
    ggplot(datatoplot, aes_string(x="month", y="quantity_sold_kg", group = plot_by, colour = plot_by)) +
        geom_line() +
        theme_bw()
}
#TEST

# master_train <- readRDS("master_train.RData")
# master_train$productID <- as.factor(master_train$productID)
# master_train$storeID <- as.factor(master_train$storeID)
# t <- head(master_train[, .(storeID, productID)])
# plotsales(t, plot_by = "storeID", sales_by_month)
# plotsales(master_train[, .(storeID, productID)], plot_by = "community", sales_by_month)


#stacked barplot
library(ggplot2)
library(data.table)
master_table <- readRDS("master_table.RData")
master_table$chain <- as.factor(master_table$chain)
master_table$sub_chain <- as.factor(master_table$sub_chain)

ggplot(master_table, aes(x = chain, y = total_quantity, fill=sub_chain, group=sub_chain)) + 
    geom_bar(stat = "summary", fun.y=sum)