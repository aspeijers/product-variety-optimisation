library(data.table)
library(lubridate)

sales<-readRDS("/home/didi/BGSE/semester3/kernel/data/sales.RData")
# Match the date to the assortment files
sales = sales[date>"2014-12-31"]

## Checking if we have two entries for the same product on the same date and for the same store 
a = sales[,.(daily_records=length(date)),by=.(storeID,productID)]
b = sales[,.(uni_daily_records=length(unique(date))),by=.(storeID,productID)]
sum(a$daily_records!=b$uni_daily_records)

# Result - NO


## Read in the assortment table
assortment_date = readRDS("/home/didi/BGSE/semester3/kernel/data/product_store_timeline_total_days_sales.RData")
assortment_date[,store_product_ID := paste0(storeID,productID)]
# aggregate by time 
sales_agg_time = sales[,.(quantity_sold_kg_agg = sum(quantity_sold_kg)),by = .(productID,storeID)]
sales_agg_time[,store_productID := paste0(storeID,productID)]

# Does stores and products match in both sets
ass_store = length(unique(assortment_date[,storeID]))
s_store = length(unique(sales_agg_time[,storeID]))
s_store - ass_store  #148
# More stores in sales records

ass_product = length(unique(assortment_date[,productID]))
s_product = length(unique(sales_agg_time[,productID]))
s_product - ass_product #-54
# more products in assortment 

# Take only the mutual entries 
sales_agg_time = sales_agg_time[store_productID %in% assortment_date[,store_product_ID]]
# Note - we still have more products in assortment 

# Now let's normalize 
# initialize empty vector
average_sales=rep(NA,nrow(sales_agg_time))
sales_agg_time = as.data.frame(sales_agg_time)
sales_agg_time = cbind(sales_agg_time,average_sales)

# Average Sales (divide by number of days) 
for ( i in 1:nrow(sales_agg_time) ) {
  id = sales_agg_time[i,"store_productID"]
  id_tot_days = assortment_date[store_product_ID == id, total_days]
  quantity = sales_agg_time[i,"quantity_sold_kg_agg"]
  sales_agg_time[i,"average_sales"] <-  quantity/id_tot_days 
}
# this takes around 15mins to run!


# Average Sales by sub_chain
# load store data
stores <- readRDS("/home/didi/BGSE/semester3/kernel/data/stores.RData")
sub_chain = rep(NA,nrow(sales_agg_time))
for (i in 1:nrow(sales_agg_time)){
  id = sales_agg_time[i,"storeID"]
  sub_chainID = stores[storeID ==id, sub_chain][1]
  sub_chain[i] = sub_chainID
}

sales_agg_time = cbind(sales_agg_time,sub_chain)

# Average Sales by product subFamily
# load product and find subfamily for each product
products = readRDS("/home/didi/BGSE/semester3/kernel/data/products.RData")
subFam = rep(NA,nrow(sales_agg_time))
for (i in 1:nrow(sales_agg_time)){
  id = sales_agg_time[i,"productID"]
  subFamID = products[productID ==id, subFam]
  subFam[i] = subFamID
} 

sales_agg_time = cbind(sales_agg_time,subFam)
sales_agg_time = as.data.table(sales_agg_time)

#average by sub_chain and subFam
sales_agg_time_sub_chain_subFam = sales_agg_time[,.(agg_sales = mean(average_sales)), by =.(sub_chain,subFam)]
saveRDS(sales_agg_time_sub_chain_subFam,file = "/Users/annekespeijers/Desktop/BGSE/Term3/MasterProject/GSE/sales_agg_time_subChain_subFam.RData")





