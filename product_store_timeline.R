library(data.table)

# path to the variety files 
path = "/home/didi/BGSE/semester3/kernel/data/assortment/"
setwd(path)

# extract the file names 
file.names <- dir(path, pattern =".RData")
# remove 20160315 file due to sales compatability 
file.names = file.names[-length(file.names)]

#initialize empty data table
product_store_timeline = data.table(storeID = integer(0),productID = character(0))

#loop through names 
for(i in 1:length(file.names)){
  #read the file
  file <- readRDS(file.names[i])
  # take the products which are in the store
  file = file[shelf_space!=0]
  # remove duplicated entries
  file = file[,by=.(storeID,productID)]
  # remove shelf_space
  file = file[,shelf_space:=NULL]
  # take only the unique combination (like a set)
  product_store_timeline = unique(rbind(product_store_timeline,file))
}

# Create new variable which consists of storeID+productID
product_store_timeline[,store_product_ID:=paste(storeID,productID,sep = "")]


# loop through dates

for(i in 1:length(file.names)){
  file <- readRDS(file.names[i])
  file = file[shelf_space!=0]
  file = file[,by=.(storeID,productID)]
  file = file[,shelf_space:=NULL]
  file[,store_product_ID:=paste(storeID,productID,sep = "")]
  
  # check if store_product combination exists on a particular day and return Boolean indicator
  product_store_timeline[,col4:=ifelse(product_store_timeline[,store_product_ID]
                                       %in% file[,store_product_ID],1,0)]
  
  # Rename the column to be equl to the date 
  date = regmatches(file.names[i], regexpr("[0-9].*[0-9]", file.names[i]))
  names(product_store_timeline)[ncol(product_store_timeline)]<-date
  
}


total_days = rowSums(product_store_timeline[,4:441, with = FALSE])

# add it to the data table
product_store_timeline_total_days = as.data.frame(product_store_timeline[,.(storeID,productID)])
product_store_timeline_total_days = as.data.table(cbind(product_store_timeline_total_days,total_days))

# just point to where you want to save it
# the file below has storeID,productID and a boolean column for each of the dates in the assortment 
saveRDS(product_store_timeline,file = "/home/didi/BGSE/semester3/kernel/data/product_store_timeline.RData")
# the file below has the storeID, productID and the total number of days it was sold  
saveRDS(product_store_timeline_total_days,file = "/home/didi/BGSE/semester3/kernel/data/product_store_timeline_total_days.RData")
