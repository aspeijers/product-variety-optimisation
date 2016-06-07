library(data.table)
library(sampling)
library(plyr)

setwd("/media/balint/Storage/Tanulas/thesis/product-variety-optimisation")
# setwd("~/Desktop/BGSE/Term3/MasterProject/GSE")

########################## Split by stores ###################################
# read in master table
master <- readRDS("master_table.RData")

dplyr(master, .(sub_chain, chain, town, size, community, province, subFam, fam, grup), function(d) sample(nrow(d)))
test <- ddply(master, .(sub_chain, chain, town, size, community, province), function(d) { d[sample(nrow(d), pmin(nrow(d), 50)), ]})

train <- setdiff(master, test)





## old code

# stores <- unique(master[,.(size, storeID)])
# 
# # What do we do with the NA's?????????? Omit them for now.
# stores <- na.omit(stores)
# 
# # sort
# stores <- stores[order(stores$size),]
# table(stores$size)
# 
# # .75*29
# # .75*130
# # .75*79
# # .75*611
# # .75*997
# # .75*658
# # .75*36
# qtys <- c(22, 97, 59, 485, 748, 493, 27)
# 
# s <- strata(stores, "size", qtys, description=TRUE)
# training_stores <- getdata(stores, training_stores)
# #table(training_stores$size)
# training_stores <- training_stores$storeID
# 
# master_train <- master[storeID %in% training_stores]
# master_test <- master[!(storeID %in% training_stores)]
# 
# saveRDS(master_train, file="master_train.RData")
# saveRDS(master_test, file="master_test.RData")




