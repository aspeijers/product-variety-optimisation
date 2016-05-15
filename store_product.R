library(data.table)

# path to the variety files 
path = "/home/didi/BGSE/semester3/kernel/data/assortment/"
# path = "~/Desktop/BGSE/Term3/MasterProject/GSE/old/assortment"
#path = "/media/balint/Storage/Tanulas/thesis/product-variety-optimisation/assortment/"
setwd(path)

# extract the file names 
file.names <- dir(path, pattern =".RData")
# remove 20160315 file due to sales compatability 
file.names = file.names[-length(file.names)]

store_product = readRDS("/home/didi/BGSE/semester3/kernel/data/product_store_timeline_total_days.RData") 
store_product[,total_days:=NULL]

#create temporary table through which to loop
store_product_temp = store_product

#loop through all files
for(i in 1:length(file.names)){
  print(i)
  file <- readRDS(file.names[i])
  file = file[file$shelf_space != 0]
  file = file[,by=.(storeID,productID)]
  file[,store_product_ID:=paste(storeID,productID,sep = "")]
  file[,c("storeID","productID"):=list(NULL,NULL)]
  
  #left Join
  # Take cares of the alltimer issue, too
  store_product_temp = merge(x = store_product_temp, y = file, by = "store_product_ID", all.x = TRUE)
  store_product_temp$shelf_space = ifelse(is.na(store_product_temp$shelf_space),0,store_product_temp$shelf_space)
  
  # Rename the column to be equl to the date 
  date = regmatches(file.names[i], regexpr("[0-9].*[0-9]", file.names[i]))
  #store_product[,new := shlef]
  #store_product$new = store_product_temp$shelf_space
  store_product = cbind(store_product,store_product_temp$shelf_space)
  names(store_product)[ncol(store_product)] = date
  store_product_temp[,shelf_space:= NULL]
}


saveRDS(store_product,"../store_product.RData") 
### define mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


### Divide it into 2 parts for memory reasons
n =nrow(store_product)
# first part
rm(store_product)
store_product_half = store_product[1:(n/2),]
#total shelf space
store_product_half[,total_shelf_space := rowSums(store_product_half[,5:441, with = FALSE])]
store_product_half[,average_shelf_space := (total_shelf_space/total_days)]
store_product_half[,mode_shelf_space := names(sort(-table((store_product_half[,5:441, with = FALSE]))))[1]]
#second part
store_product = readRDS("../store_product.RData")
rm(store_product)
store_product_half2 = store_product[((n/2)+1):n,]
#total shelf_space
store_product_half2[,total_shelf_space := rowSums(store_product_half2[,5:441, with = FALSE])]
store_product_half2[,average_shelf_space := (total_shelf_space/total_days)]

saveRDS(store_product_half2,"/home/didi/BGSE/semester3/kernel/data/store_product_hald2.RData")
#read the first half
store_product_half = readRDS("/home/didi/BGSE/semester3/kernel/data/store_product_half.RData")
#combine both
store_product = rbind(store_product_half,store_product_half2)
saveRDS(store_product,"/home/didi/BGSE/semester3/kernel/data/store_product.RData")










