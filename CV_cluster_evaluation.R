##### Evaluate clusters
setwd("~/BGSE/semester3/kernel/data")
library(data.table)
library(plyr)
train = readRDS("store_train.RData")
test = readRDS("store_test.RData")
clusters = readRDS("kmeans_clusters.RData")
cluster = clusters[,1:2]
rm(clusters)
names(cluster)[2] ="cluster"

#identify which grup has only 1 or 2 or 3 products 
products = readRDS("products.RData")
products = products[availability == "alltimer"]
group_statistics  = products[,.(num_products = length(productID)), by = grup]
avg_groups = c()
for(i in group_statistics$grup){
  if (group_statistics[grup==i,]$num_products == 1 |
      group_statistics[grup==i,]$num_products == 2 |
      group_statistics[grup==i,]$num_products == 3){
    avg_groups = c(avg_groups,i)
  }
}


#identify which flavor has only 1 or 2 or 3 products 
flavor_statistics  = products[,.(num_products = length(productID)), by = flavor]
avg_flavor= c()
for(i in flavor_statistics$flavor){
  if (flavor_statistics[flavor==i,]$num_products == 1 |
      flavor_statistics[flavor==i,]$num_products == 2 |
      flavor_statistics[flavor==i,]$num_products == 3){
    avg_flavor = c(avg_flavor,i)
  }
}

rm(products,flavor_statistics,group_statistics)



mkt_MSE = function(train,test, cluster,avg_flavor,avg_groups){
  ### add the clustering to the train and test data 
  cluster$storeID = as.integer(as.character(cluster$storeID))
  train = merge(train, cluster, by = "storeID",all.x = TRUE)
  test = merge(test,cluster, by = "storeID", all.x = TRUE)
  rm(cluster)
  
  ### w.r.t. product 
  print("product")
  avg_variables = c("mkt_product_store","mkt_Fam_store")
  train_product = train[,lapply(.SD,mean), by =.(cluster,productID),.SDcols = avg_variables ]
  names(train_product)[3:ncol(train_product)] = paste0(avg_variables,"_predict")
  #merge to test 
  test = merge(test,train_product,by=c("cluster","productID"), all.x = TRUE)
  rm(train_product)
  
  
  #### w.r.t. subFam
  print("subFam")
  avg_variables = c("mkt_subFam_store","mkt_subFam_store_sales")
  train_subFam = train[,lapply(.SD,mean), by =.(cluster,subFam),.SDcols = avg_variables ]
  names(train_subFam)[3:ncol(train_subFam)] = paste0(avg_variables,"_predict")
  #merge to test 
  test = merge(test,train_subFam,by=c("cluster","subFam"), all.x = TRUE)
  rm(train_subFam)
  
  ### w.r.t. fam
  print("fam")
  avg_variables = c("mkt_Fam_store","mkt_Fam_store_sales")
  train_fam = train[,lapply(.SD,mean), by =.(cluster,fam),.SDcols = avg_variables ]
  names(train_fam)[3:ncol(train_fam)] = paste0(avg_variables,"_predict")
  #merge to test 
  test = merge(test,train_fam,by=c("cluster","fam"), all.x = TRUE)
  rm(train_fam)
  
  
  ### w.r.t. grup 
  print("grup")
  # subset training data for grup that needs to be averaged over all stores in a particular cluster
  temp = train[grup%in%avg_groups]
  train_grup_average = temp[,.(mkt_Grup_store_predict=mean(mkt_Grup_store),
                               mkt_Grup_store_sales_predict=mean(mkt_Grup_store_sales))
                            , by = .(cluster,grup)]
  #artificially create stores
  store_cluster_grup = test[,.(storeID,grup,cluster)]
  #there are duplicates here, take the unique rows
  store_cluster_grup = unique(store_cluster_grup)
  store_cluster_grup = store_cluster_grup[grup%in%avg_groups]
  #merge the averaging estimation for every store
  train_grup_average = merge(store_cluster_grup,train_grup_average, by = c("cluster","grup"),all.x = TRUE)
  #remove cluster
  train_grup_average = train_grup_average[,cluster:=NULL]
  rm(store_cluster_grup)
  rm(temp)
  
  #subset for the ones which are not in avering part
  temp2 = train[!(grup%in%avg_groups)]
  train_grup_not_average = temp2[,.(mkt_Grup_store_predict=unique(mkt_Grup_store),
                                    mkt_Grup_store_sales_predict=mean(mkt_Grup_store_sales)), 
                                 by = .(storeID,grup)]
  rm(temp2)
  
  #merge averaging and not-averaging
  mkt_Grup_store_predict = rbind(train_grup_not_average,train_grup_average)
  rm(train_grup_not_average,train_grup_average)
  
  # If NAs occur take the average across cluster
  if (is.na(mkt_Grup_store_predict$mkt_Grup_store_predict )| 
      is.na(mkt_Grup_store_predict$mkt_Grup_store_sales_predict)) {
    train_grup_average = train[,.(storeID,mkt_Grup_store_predict=mean(mkt_Grup_store),
                                 mkt_Grup_store_sales_predict=mean(mkt_Grup_store_sales))
                              , by = .(cluster,grup)]
    
  }

  

  #merge to test 
  test = merge(test,mkt_Grup_store_predict,by=c("storeID","grup"), all.x = TRUE)
  rm(mkt_Grup_store_predict)
  
  
  
  
  
  ### w.r.t. flavor 
  print("flavor")
  # subset training data for var that need to be averaged over all stores in a particular cluster
  temp = train[flavor%in%avg_flavor]
  train_flavor_average = temp[,.(mkt_Flavor_store_predict=mean(mkt_Flavor_store),
                               mkt_Flavor_store_sales_predict=mean(mkt_Flavor_store_sales))
                            , by = .(cluster,flavor)]
  #artificially create stores
  store_cluster_flavor = test[,.(storeID,flavor,cluster)]
  #there are duplicates here, take the unique rows
  store_cluster_flavor = unique(store_cluster_flavor)
  store_cluster_flavor = store_cluster_flavor[flavor%in%avg_flavor]
  #merge the averaging estimation for every store
  train_flavor_average = merge(store_cluster_flavor,train_flavor_average, by = c("cluster","flavor"),all.x = TRUE)
  #remove cluster
  train_flavor_average = train_flavor_average[,cluster:=NULL]
  rm(store_cluster_flavor)
  rm(temp)
  
  #subset for the ones which are not in avering part
  temp2 = train[!(flavor%in%avg_flavor)]
  train_flavor_not_average = temp2[,.(mkt_Flavor_store_predict=unique(mkt_Flavor_store),
                                    mkt_Flavor_store_sales_predict=mean(mkt_Flavor_store_sales)), 
                                 by = .(storeID,flavor)]
  rm(temp2)
  
  #merge averaging and not-averaging
  mkt_Flavor_store_predict = rbind(train_flavor_not_average,train_flavor_average)
  rm(train_flavor_not_average,train_flavor_average)
  
  #merge to test 
  test = merge(test,mkt_Flavor_store_predict,by=c("storeID","flavor"), all.x = TRUE)
  rm(mkt_Flavor_store_predict)
  
  ### w.r.t. type 
  print("type")
  train_type = train[,.(mkt_Type_store_predict=unique(mkt_Type_store),
                                      mkt_Type_store_sales_predict=mean(mkt_Type_store_sales)), 
                                   by = .(storeID,type)]
  #merge to test 
  test = merge(test,train_type,by=c("storeID","type"), all.x = TRUE)
  rm(train_type)
  
  ### w.r.t. units 
  print("units")
  train_units = train[,.(mkt_Units_store_predict=unique(mkt_Units_store),
                        mkt_Units_store_sales_predict=mean(mkt_Units_store_sales)), 
                     by = .(storeID,units)]
  #merge to test 
  test = merge(test,train_units,by=c("storeID","units"), all.x = TRUE)
  rm(train_units)
  rm(avg_variables)
  
  
  
  
  
  ### Calculating MSE
  mse_vector = rep(NA,14)
  names(mse_vector) = names(test)[27:40]
  
  mse_vector[1] = sum((test$mkt_product_store - test$mkt_product_store_predict)^2)/nrow(test)
  mse_vector[2] = sum((test$mkt_subFam_store - test$mkt_subFam_store_predict)^2)/nrow(test)
  mse_vector[3] = sum((test$mkt_Fam_store - test$mkt_Fam_store_predict)^2)/nrow(test)
  mse_vector[4] = sum((test$mkt_Grup_store - test$mkt_Grup_store_predict)^2)/nrow(test)
  mse_vector[5] = sum((test$mkt_Flavor_store - test$mkt_Flavor_store_predict)^2)/nrow(test)
  mse_vector[6] = sum((test$mkt_Type_store - test$mkt_Type_store_predict)^2)/nrow(test)
  mse_vector[7] = sum((test$mkt_Units_store - test$mkt_Units_store_predict)^2)/nrow(test)
  mse_vector[8] = sum((test$mkt_product_store_sales- test$mkt_product_store_sales_predict)^2)/nrow(test)
  mse_vector[9] = sum((test$mkt_subFam_store_sales - test$mkt_subFam_store_sales_predict)^2)/nrow(test)
  mse_vector[10] = sum((test$mkt_Fam_store_sales - test$mkt_Fam_store_sales_predict)^2)/nrow(test)
  mse_vector[11] = sum((test$mkt_Grup_store_sales - test$mkt_Grup_store_sales_predict)^2)/nrow(test)
  mse_vector[12] = sum((test$mkt_Flavor_store_sales - test$mkt_Flavor_store_sales_predict)^2)/nrow(test)
  mse_vector[13] = sum((test$mkt_Type_store_sales - test$mkt_Type_store_sales_predict)^2)/nrow(test)
  mse_vector[14] = sum((test$mkt_Units_store_sales - test$mkt_Units_store_sales_predict)^2)/nrow(test)

 return(mse_vector) 
}

##############################################################################################################################
##############################################################################################################################

#the CV_represent function 
clusters = readRDS("kmeans_clusters.RData")
master = readRDS("master_mktshare.RData")


CV_represent = function(master_table, number_of_CV,clusters_df){
## initialize empty list to keep the mse's
mse_list = list()
## Loop through the different train and test data  
 for (i in 1:number_of_CV){
   master <- master[,storeprodID:=paste0(storeID, productID)]
   
   #make sure that all test storeIDs already exist in train data
   none_new_stores = FALSE
   while (none_new_stores == FALSE){
     
     # split into test and train by (storeID, productID), using representative sampling method (for stores)
     test    <- ddply(master, .(chain, town, size, community, province), function(d) { d[sample(nrow(d), pmin(nrow(d), 20)), ]})
     test <- as.data.table(test)
     
     train_rows   <- setdiff(master[,storeprodID], test[,storeprodID])
     train <- master[storeprodID %in% train_rows]
     rm(train_rows)
     
     # remove extra column and master table
     test[,storeprodID:=NULL]
     train[,storeprodID:=NULL]
     #check if all stores in test are present in train
    none_new_stores = sum( unique(test$storeID) %in% unique(train$storeID) ) == length(unique(test$storeID))
   }
  
   
   # in the particular train and test, loop through the different clusters
   #we store the mse cluster values in a matrix - initialize it 
   mse = c()
   #vector containing the row names of the above matrix  
   rnames = c()
   for(j in 2:length(clusters_df)){
     #isolate cluster
     cluster = clusters_df[,c(1,j)]
     #calculate mse using the above function
     new_mse = mkt_MSE(train,test, cluster,avg_flavor,avg_groups)
     #append to the mse matrix
     mse = rbind(mse,new_mse)
     rnames = c(rnames, paste0("cluster",j))
   }
   #name the columns appropriately, indicating which mse we are taking 
   colnames(mse) = names(new_mse)
   rownames(mse) = rnames 
   #save it in the list
   mse_list[[i]] = as.data.frame(mse)
   
 }
  
return(mse_list)
}




