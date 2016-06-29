##################################################################################################
######## Description: Creating product-store timeline with respect to shelf space     
######## input: product_store_timeline_total_days.RData
######## output: store_product.RData
##################################################################################################


library(data.table)

path = "/assortment"

# extract the file names 
file.names <- dir(path, pattern =".RData")
# remove 20160315 file due to sales compatability 
file.names = file.names[-length(file.names)]

store_product = readRDS("../product_store_timeline_total_days.RData") 
#store_product[,total_days:=NULL]

#create temporary table through which to loop
store_product_temp = store_product

# loop through all files and take the shelf space 
for(i in 1:length(file.names)){
  print(i)
  file <- readRDS(file.names[i])
  file <- file[shelf_space != 0]
  # in case of duplicates take the first one
  file <- file[,by=.(storeID,productID)]
  file[,store_product_ID:=paste(storeID,productID,sep = "")]
  file[,c("storeID","productID"):=list(NULL,NULL)]
  
  #left Join
  store_product_temp = merge(store_product_temp, file, by = "store_product_ID", all.x = TRUE)
  store_product_temp$shelf_space = ifelse(is.na(store_product_temp$shelf_space), 0, store_product_temp$shelf_space)
  
  # Rename the column to be equl to the date 
  date = regmatches(file.names[i], regexpr("[0-9].*[0-9]", file.names[i]))
  store_product = cbind(store_product,store_product_temp$shelf_space)
  names(store_product)[ncol(store_product)] = date
  store_product_temp[,shelf_space:= NULL]
}

# intermediate save (just in case)
#saveRDS(store_product,"../store_product.RData") 
rm(file, store_product_temp, date, file.names, i, path)


# Total shelf space: calculate total shelf space for each (storeID, productID) 
store_product[,total_shelf_space := rowSums(store_product[,5:442, with = FALSE])]

# Mode: calculate shelf space mode for each (storeID, productID) 
store_product[,mode_shelf_space := NA_integer_]
# NB. This loop takes 2-3 hrs to run. 
for(i in 1:nrow(store_product)) {
    print(i)
    row = as.numeric(as.vector(store_product[i,5:442, with=F]))
    mode = unique(row)[which.max(tabulate(match(row,unique(row))))] 
    store_product[i,mode_shelf_space:=mode]
} 

# save final store_product table
saveRDS(store_product,"../store_product.RData") 




