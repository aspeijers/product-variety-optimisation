library(data.table)
# Add the promotional group 
stores_old = readRDS("pdvClean.RData")
stores_new= readRDS("stores.RData")


names(stores_old)[1:2] = c("storeID","promo_group")
stores_old[,names(stores_old)[3:length(names(stores_old))]:=NULL]

stores_new = setorder(stores_new,-storeID)
stores_old = setorder(stores_old,-storeID)
stores_new = cbind(stores_new,stores_old$promo_group)

# Look which stores we need 
master_train = readRDS("master_train_mktshare.RData")
master_test = readRDS("master_test_mktshare.RData")
#take the storeIDs which are common for both train and test
storeID_master = unique(c(master_test$storeID,master_train$storeID))

rm(master_test)
rm(master_train)
rm(stores_old)

#subset stores for the ones which we want 
stores_new = stores_new[storeID %in% storeID_master]


#find the not-changing-promo_group storeIDs
not_changing_promo_group_stores = c()
for(i in storeID_master){
  if(length(unique(stores_new[storeID ==i,]$promo_group)) == 1){ 
    changing_promo_group_stores=c(changing_promo_group_stores,i)
    }
}

#subset only for those whose promo_group is not changing 
stores_new = stores_new[storeID %in% not_changing_promo_group_stores]

#take only one row for each store (nothing is changing now)
stores_new[,date:=NULL]
stores_new = stores_new[duplicated(stores_new)==FALSE]

#save it 
saveRDS(stores_new,"stores_promo_group.RData" )

#how many promotiona groups there are 
length(unique(stores_new$promo_group))

#first remove the promo_group where there are less than 2 stores 
promo_group_filtered = as.data.frame(table(stores_new$promo_group))
promo_group_filtered = promo_group_filtered[promo_group_filtered$Freq>2,]

#take the first promo group 
promo1 = as.integer(as.character(promo_group_filtered[1,1]))
stores_promo1 = stores_new[promo_group == promo1]$storeID
#load in the sales data 
sales = readRDS("sales.RData")
#subset for promo1
sales_promo1 = sales[storeID %in% stores_promo1]

common_products = c()
for(i in unique(sales_promo1$productID)){
  if(length(unique(sales_promo1[productID==i]$storeID))==33){
    common_products = c(common_products,i)
  }
}

### Plot them 

#sales_by_month <- readRDS("sales_by_month.RData")
library(ggplot2)
library(plyr)
library(data.table)
#this function is for plotting sales as time series
#sales_by_month is the data frame needed
#id_pairs id the data frame with productID and storeID pairs
#plot_by is the factor variable by which you want to split the data
plotsales <- function(id_pairs, plot_by, sales_by_month){
  datatoplot <- merge(id_pairs, sales_by_month, by = c("storeID", "productID"))
  #datatoplot <- ddply(datatoplot, .(get(plot_by), month), summarize, quantity_sold_kg = sum(quantity_sold_kg))
  #names(datatoplot)[1] <- plot_by
  ggplot(datatoplot, aes_string(x="date", y="quantity_sold_kg", group = plot_by, colour = plot_by)) +
    geom_line() +
    theme_bw()
}
#TEST

product1 = common_products[1]
id_pairs = cbind(stores_promo1,rep(product1,length(stores_promo1)))
colnames(id_pairs) = c("storeID","productID")
id_pairs = as.data.frame(id_pairs)
id_pairs$storeID = as.factor(id_pairs$storeID)
 id_pairs$productID = as.factor(id_pairs$productID)
 sales_promo1$productID = as.factor(sales_promo1$productID)
 sales_promo1$storeID = as.factor(sales_promo1$storeID)
 plotsales(id_pairs, plot_by = "storeID", sales_promo1)
 
 plotsales(id_pairs, plot_by = "storeID", sales_promo1)
 
 #group stores by something else
 # size of the store 
 # divide by total sales 
 
 







