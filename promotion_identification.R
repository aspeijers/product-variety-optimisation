library(data.table)
library(ggplot2)
stores= readRDS("stores.RData")

# Look which stores we need 
master_train = readRDS("master_train_mktshare.RData")
master_test = readRDS("master_test_mktshare.RData")
#take the storeIDs which are common for both train and test
storeID_master = unique(c(master_test$storeID,master_train$storeID))

rm(master_test)
rm(master_train)


#subset stores for the ones which we want 
stores= stores[storeID %in% storeID_master]


#find the not-changing-promo_group storeIDs
not_changing_promo_group_stores = c()
for(i in storeID_master){
  if(length(unique(stores[storeID ==i,]$promo_group)) == 1){ 
    not_changing_promo_group_stores=c(not_changing_promo_group_stores,i)
    }
}

#subset only for those whose promo_group is not changing 
stores = stores[storeID %in% not_changing_promo_group_stores]

#take only one row for each store (nothing is changing now)
stores[,date:=NULL]
stores = stores[duplicated(stores)==FALSE]

#save it 
saveRDS(stores,"stores_promo_group.RData" )

#how many promotion groups there are 
length(unique(stores$promo_group))

#first remove the promo_group where there are less than 2 stores 
promo_group_filtered = as.data.frame(table(stores$promo_group))
promo_group_filtered = promo_group_filtered[promo_group_filtered$Freq>2,]

#take the first promo group 
promo1 = as.integer(as.character(promo_group_filtered[1,1]))
stores_promo1 = stores[promo_group == promo1]$storeID
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


 ##### t+1/t
product1 = common_products[1]
 


simple_ratio_function = function(x){
  simple_ratio = x/shift(x,type="lag")
}

sales_promo1_pr1 = sales_promo1_pr1[,.(date,quantity_sold_kg,simple_ratio = simple_ratio_function(quantity_sold_kg)), by = storeID]


  ggplot(sales_promo1_pr1, aes(x=date, y=simple_ratio, group = as.character(storeID), colour = as.character(storeID))) +
    geom_line() +
    theme_bw()

#try to spread the sales over certain period 
# daily contdata

a = data.table(date=seq.Date(min(sales_promo1_pr1$date), max(sales_promo1_pr1$date), by="day"))
storeID = rep(stores_promo1,each = nrow(a))
a_all_stores = cbind(storeID,a)
rm(storeID)
b = merge(sales_promo1_pr1, a_all_stores, by=c("date","storeID"), all.y = TRUE)
#change all NAs to zeros 
b$quantity_sold_kg = ifelse(is.na(b$quantity_sold_kg),0,b$quantity_sold_kg)
#smoothing part 
 
smoothing = function(x,time_period){
  daily_sells = 0 
  for (i in 1:time_period){
    new_enty = shift(x,n=i,type = "lag")/time_period
    daily_sells = daily_sells + new_enty
  }
  return(daily_sells)
}

time_period = 14
b = b[,quantity_b:=NA_real_]
b = b[,.(date,quantity_sold_kg,quantity_b = smoothing(quantity_sold_kg,time_period)), by = storeID]

ggplot(b, aes(x=date, y=quantity_b, group = as.character(storeID), colour = as.character(storeID))) +
  geom_line() +
  theme_bw()  

b = b[,.(date,quantity_sold_kg,quantity_b,simple_ratio = simple_ratio_function(quantity_b)), by = storeID]

ggplot(b, aes(x=date, y=simple_ratio, group = as.character(storeID), colour = as.character(storeID))) +
  geom_line() +
  theme_bw() 


### median_ratio

median_ratio_function = function(x,median_period){
  values = as.data.frame(matrix(NA,length(x),median_period))
  for (i in 1:median_period){
    new_enty = shift(x,n=i,type = "lag")
    values[,i] = new_enty
  }
  med = apply(values,1,median)
  med = ifelse(is.na(med),x,med)
  med = ifelse(is.na(med),0,med)
  median_ratio = x/med
  median_ratio = ifelse(is.nan(median_ratio),1,median_ratio)
  median_ratio = ifelse(is.infinite(median_ratio),0,median_ratio)
  median_ratio = ifelse(is.na(median_ratio),0,median_ratio)
  return(median_ratio)
}

median_period = 7
sales_promo1_pr1 = sales_promo1_pr1[,.(date,quantity_sold_kg,median_ratio = median_ratio_function(quantity_sold_kg,median_period)), by = storeID]
ggplot(sales_promo1_pr1, aes(x=date, y=median_ratio, group = as.character(storeID), colour = as.character(storeID))) +
  geom_line() +
  theme_bw() 

b = b[,.(date,quantity_sold_kg,quantity_b,median_ratio = median_ratio_function(quantity_b,median_period)), by = storeID]
ggplot(b, aes(x=date, y=median_ratio, group = as.character(storeID), colour = as.character(storeID))) +
  geom_line() +
  theme_bw() 

##looking at the sales_promo1
above = quantile(sales_promo1_pr1$median_ratio,probs =0.95)
sales_promo1_pr1 = sales_promo1_pr1[,extremes := median_ratio > above]
extreme_events = sales_promo1_pr1[extremes == TRUE]
setorder(extreme_events,date)
#date grouping 
cluster_dates = function(x){
  clusters = rep(NA,length(x))
  #clusters[1] = 1 
  a = diff(extreme_events$date,lag = 1)
  clusters = ifelse(a>3,1,0)
  
  index=1
  cluster_name = rep(NA,length(clusters))
  for (i in 1:length(clusters)){
    if (clusters[i]==0){
      cluster_name[i] = index
    }else{
      cluster_name[i] = index +1
      index = index +1 
    }
  }
}


## the smooth one 
above = quantile(b$median_ratio,probs =0.99)
b = b[,extremes := median_ratio > above]
extreme_events = b[extremes == TRUE]
setorder(extreme_events,date)


