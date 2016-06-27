library(data.table)
stores = readRDS("stores.RData")
stores = as.data.frame(stores)

missing = as.data.frame(matrix(NA,2,ncol(stores)))
missing[1,]=names(stores)
for(i in 1:ncol(stores)){
    missing[2,i]=(sum(is.na(stores[,i]))/nrow(stores)) 
}
