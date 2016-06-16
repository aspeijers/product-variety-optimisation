# suggestion function

# this ranks the products by their predicted mkt share and returns the suggested products
# no substitution effects

# read in data
master <- readRDS("master_mktshare.RData") # note. these are all alltimers.

# count number of unique products in each store
no_products <- master[,.(no_products = length(unique(productID))), by=storeID] 


#read in the predicted mktshare data table
predicted_mktshare = readRDS("predicted_mktshare.RData")

suggestions <- c()

for(st in unique(predicted_mktshare$storeID)){
    print(st)
    preds <- predicted_mktshare[storeID==st, ]
    
    # order by predicted market share
    setorder(preds, -pred)
    limit <- no_products[storeID==st,]$no_products
    
    # check for ties
    if (length(unique(preds$pred)) != 107) {
        print("ties found")
        break
    }
    
    suggestions <- rbind(suggestions, preds[1:limit, ])
}
#save
saveRDS(suggestions,"suggestions.RData")


# check how many existing and how many new products suggested for each store
# percentage

product_differences <- c()

for (st in unique(suggestions$storeID)) {
     no_new = length(setdiff(master[storeID == st,]$productID,suggestions[storeID == st,]$productID))
     product_differences = rbind(product_differences, c(st,no_new,no_products[storeID==st,]$no_products))

}
# rename the column names
colnames(product_differences) = c("storeID","no_new","total_no_products")
# calculate percentage change of new products
product_differences = as.data.table(product_differences)
product_differences = product_differences[,new_products_percentage := no_new/total_no_products]

#save 
saveRDS(product_differences,"product_differences.RData")


