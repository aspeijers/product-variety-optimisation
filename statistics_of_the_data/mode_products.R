##################################################################################################
######## Description: Finding the mode number of products per each store  
######## input: product_store_timeline.RData; products.RData
######## output:
##################################################################################################



library(data.table)
#load the data
productstore_timeline = readRDS("product_store_timeline.RData")

#for every 2 weeks period sum the present products
store_product_tuple = productstore_timeline[,.(storeID,productID)]
#transform it to data frame for convenience
productstore_timeline = as.data.frame(productstore_timeline)
for(i in seq(4,434,by=14)){
  new = productstore_timeline[,i] 
  for(j in 1:6){
    k = i+j
    new = productstore_timeline[,k] + new
  }
store_product_tuple = cbind(store_product_tuple,new)

}
#for the rest which do not fit in 2 weeks
new = productstore_timeline[,434]
for(j in 1:6){
  k = 434+j
  new = productstore_timeline[,k] + new
}
store_product_tuple = cbind(store_product_tuple,new)

#change the names
names(store_product_tuple)[3:34] = c("w1","w2","w3","w4","w5","w6","w7","w8","w9","w10",
                                     "w11","w12","w13","w14","w15","w16","w17","w18","w19",
                                     "w20","w21","w22","w23","w24","w25","w26","w27","w28",
                                     "w29","w30","w31","w32")

#if they are present in the store_product_tuple than we put one otherwise we put zero
product_in_stores = as.data.table(as.data.frame(store_product_tuple))

#loop through the store_product_tuples to write 0/1 values
product_in_stores = as.data.table(ifelse(store_product_tuple!=0,1,0))
product_in_stores[,storeID:=NULL]
product_in_stores[,productID:=NULL]
product_in_stores = as.data.table(cbind(store_product_tuple$storeID,product_in_stores))
names(product_in_stores)[1]="storeID"


#group by stores 
product_in_stores = product_in_stores[,lapply(.SD,sum), by = storeID]
#find the median 


product_in_stores = product_in_stores[,med_product:=NA]
product_in_stores = as.data.frame(product_in_stores)
for( i in 1:nrow(product_in_stores)){
  product_in_stores[i,]$med_product = median(as.numeric(product_in_stores[i,2:32]))
}

rm(productstore_timeline)

#isolate the mode
product_in_stores = product_in_stores[,c("storeID","med_product")]
med = unlist(product_in_stores[,"med_product"])
med_nonzero = med[med!=0]

#plot the histogram 
hist(med_nonzero,main = "Frequency of Number of Products\n per Store", xlab = "Number of Products")


#let's see how many are alltimers 
products= readRDS("products.RData")
products = products[,.(productID,availability)]
products$availability = ifelse(products$availability!="alltimer",NA,"alltimer")

#merge to the store_product_tuple
store_product_tuple = merge(store_product_tuple,products,by="productID",all.x = TRUE)
#remove the product column
store_product_tuple[,productID:=NULL]
#write 0/1 identification
dummies = store_product_tuple[,2:33,with=FALSE]
dummies = ifelse(dummies!=0,1,0)
#add the storeID and the availability column
alltimer_products = as.data.table(cbind(store_product_tuple[,.(storeID,availability)],dummies))
#group
alltimer_products = alltimer_products[,lapply(.SD,sum), by = .(storeID,availability)]
#find the mode
alltimer_products = alltimer_products[,med_product:=NA]
alltimer_products = as.data.frame(alltimer_products)
for( i in 1:nrow(alltimer_products)){
  alltimer_products[i,]$med_product = median(as.numeric(alltimer_products[i,3:34]))
}

#leave only the necessary columns 
alltimer_products = alltimer_products[,c("storeID","availability","med_product")]
alltimer_only = alltimer_products[!is.na(alltimer_products$availability),]
med_all = as.numeric(alltimer_only$med_product)
med_all_nonzero = med_all[med_all!=0]

hist(med_all_nonzero,main = "Frequency of Number of Alltimer Products\n per Store", xlab = "Number of Products")

